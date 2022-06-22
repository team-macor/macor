set positional-arguments

@lsp:
    cargo watch -x "build -p macor-lsp"

@docs:
    cargo doc --open
    cargo watch -x "doc"

@flamegraph FILE:
    cargo flamegraph --root -p macor -- $1

@hyperfine FILE:
    hyperfine --warmup 3 -i "$HOME/.cargo-target/release/release/macor verify -n 1 $1" "/Users/camillafaerch/Downloads/ofmc-mac.old --numSess 1 $1"

@hyperfine-all:
    just hyperfine example_programs/KeyEx1.AnB
    just hyperfine example_programs/KeyEx3.AnB
    just hyperfine example_programs/KeyEx3b.AnB
    just hyperfine example_programs/KeyEx4.AnB
    just hyperfine example_programs/KeyEx4b.AnB
    just hyperfine example_programs/KeyEx5.AnB
    just hyperfine example_programs/KeyEx5b.AnB
    just hyperfine example_programs/KeyEx5c.AnB
    just hyperfine example_programs/KeyEx6.AnB
