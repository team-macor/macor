set positional-arguments

@lsp:
    cargo watch -x "build -p macor-lsp"

@docs:
    cargo doc --open
    cargo watch -x "doc"

@flamegraph FILE:
    cargo flamegraph --root -p macor $1
    echo "<style>:root { -webkit-filter: invert(100%); filter: invert(100%); height:100vh; background:black; }</style>" >> flamegraph.svg
