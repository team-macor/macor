use dashmap::mapref::one::Ref;
use dashmap::DashMap;
use macor_fmt::prettify;
use std::sync::Arc;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::notification::PublishDiagnostics;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    sources: DashMap<Url, Arc<String>>,
    asts: DashMap<Url, Arc<macor::parse::ast::Document<String>>>,
    client: Client,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                document_formatting_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Server initialized!")
            .await;
    }

    async fn hover(&self, _: HoverParams) -> Result<Option<Hover>> {
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String("You're hovering!".to_string())),
            range: None,
        }))
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.handle_change(params.text_document.uri, params.text_document.text)
            .await
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.handle_change(
            params.text_document.uri,
            params.content_changes[0].text.clone(),
        )
        .await
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let src = self.sources.get(&params.text_document.uri).unwrap();
        let range = Range::new(
            byte_offset_to_position(&src, 0),
            byte_offset_to_position(&src, src.len()),
        );

        match prettify(&src).unwrap() {
            Some(text) => Ok(Some(vec![TextEdit::new(range, text)])),
            _ => Ok(None),
        }
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

pub fn position_to_byte_offset(src: &str, pos: Position) -> Option<usize> {
    let mut lines = pos.line;
    let mut columns = pos.character;
    src.char_indices()
        .find(|&(_, c)| {
            if lines == 0 {
                if columns == 0 {
                    return true;
                } else {
                    columns -= 1
                }
            } else if c == '\n' {
                lines -= 1;
            }
            false
        })
        .map(|(idx, _)| idx)
}
pub fn byte_offset_to_position(src: &str, byte_offset: usize) -> Position {
    if src[0..byte_offset].is_empty() {
        return Position::new(0, 0);
    }

    if src[0..byte_offset].ends_with('\n') {
        let l = src[0..byte_offset].lines().count();
        Position::new(l as _, 0)
    } else {
        let l = src[0..byte_offset].lines().count() - 1;
        let c = src[0..byte_offset].lines().last().unwrap().len();
        Position::new(l as _, c as _)
    }
}

impl Backend {
    fn span_to_range(&self, uri: &Url, span: miette::LabeledSpan) -> Option<Range> {
        let src = self.sources.get(uri)?;
        Some(Range::new(
            byte_offset_to_position(&src, span.offset()),
            byte_offset_to_position(&src, span.offset() + span.len()),
        ))
    }
    fn diagnostic_into_lsp(&self, uri: &Url, err: &dyn miette::Diagnostic) -> Vec<Diagnostic> {
        use std::fmt::Write;

        err.labels()
            .into_iter()
            .flatten()
            .map(|label| {
                let mut message = err.to_string();

                if let Some(label) = label.label() {
                    if !label.is_empty() {
                        write!(message, "\n\n{label}").unwrap();
                    }
                }

                if let Some(help) = err.help() {
                    write!(message, "\n\n{help}").unwrap();
                }

                Diagnostic {
                    range: self
                        .span_to_range(uri, label)
                        .expect("file did not exist :("),
                    severity: match err.severity() {
                        Some(miette::Severity::Error) => Some(DiagnosticSeverity::ERROR),
                        Some(miette::Severity::Warning) => Some(DiagnosticSeverity::WARNING),
                        Some(miette::Severity::Advice) => Some(DiagnosticSeverity::INFORMATION),
                        None => None,
                    },
                    message: message.clone(),
                    ..Default::default()
                }
            })
            .collect()
    }
    async fn handle_change(&self, uri: Url, text: String) {
        let text = Arc::new(text);
        self.sources.insert(uri.clone(), text.clone());
        let ast = match macor::parse_document(&text) {
            Ok(ast) => ast,
            Err(err) => {
                let diagnostics = self.diagnostic_into_lsp(&uri, &err);

                self.client
                    .send_notification::<PublishDiagnostics>(PublishDiagnosticsParams {
                        uri,
                        diagnostics,
                        version: None,
                    })
                    .await;

                return;
            }
        };

        self.asts
            .insert(uri.clone(), Arc::new(ast.clone().map(|s| s.into())));

        match macor::protocol::Protocol::new(text.to_string(), ast) {
            Ok(_) => {}
            Err(errors) => {
                let diagnostics = errors
                    .iter()
                    .flat_map(|err| self.diagnostic_into_lsp(&uri, err))
                    .collect();

                self.client
                    .send_notification::<PublishDiagnostics>(PublishDiagnosticsParams {
                        uri,
                        diagnostics,
                        version: None,
                    })
                    .await;

                return;
            }
        }

        self.client
            .send_notification::<PublishDiagnostics>(PublishDiagnosticsParams {
                uri,
                diagnostics: vec![],
                version: None,
            })
            .await;
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        sources: Default::default(),
        asts: Default::default(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
