set positional-arguments

@lsp:
    cargo watch -x "build -p macor-lsp"

@docs:
    cargo doc --open
    cargo watch -x "doc"

@flamegraph FILE:
    cargo flamegraph --root -p macor -- $1
