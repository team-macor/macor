# MACOR & OMG

> A Static Protocol Verification Tool & Protocol Generator

This is the umbrella project for both Macor and OMG. For further information about OMG, [visit the subdirectory](https://github.com/team-macor/macor/tree/main/omg).

## Macor

### Building

To build MACOR, one needs a nightly build of the Rust compiler. The recommended way is to install using [rustup](https://rustup.rs/). To set nightly as the default, run `rustup default nightly`.

To get a MACOR binary, run `cargo build -p macor --release`. The new binary should be located at `./target/release/macor`. To see the available commands run `./target/release/macor --help`.

### Possible commands

#### Printing sessions

The constraints, and initial knowledge of sessions derived from a protocol can be printed using the following command.

```sh
$ macor sessions -n 1 ./path/to/protocol.AnB
```

This command is useful for debugging the translation steps of the front-end.

#### Verifying protocols

To analyze for protocol for attacks, one can run the following command.

```sh
$ macor verify -n 1 ./path/to/protocol.AnB
```

At the moment MACOR can only check for a handful of basic attacks, so please note that the if it reports that no attack was found, it does not necessarily imply that the protocol is secure!

### IDE Language Integration

MACOR has a basic LSP implementation, allowing editors to integrate with the verifier. It supports the following features:

- Syntax Highlighting
- Error reporting of syntax errors
- Error reporting of type errors
- Display types on hover
- Formatting protocols

#### Running

Currently there is no bundled extension for any editors, but it can be run in VSCode in debug mode. To do so run navigate to the `lsp` folder, and run the following commands:

```sh
$ npm install
$ cargo build -p macor-lsp
```

Now, open the project folder in VSCode and press `F5` which should launch a copy of VSCode with the extension installed.
