# OMG (Open-source Model Generator)

> Auto-generated Rust implementations of AnB protocols

## Project structure

The OMG project is split into multiple crates:

- `omg` is the crate for the runtime parts of the generated model.
- `omg_gen` contains the CLI for generating protocols.
- `omg_crypt` contains an implementation of `Base` using [`ring`](https://github.com/briansmith/ring)
- `parse` contains the parsing infrastructure and is part of the `macor` project

## Usage

To get started with `omg`, you must first specify a protocol in AnB syntax. To generate the source code, navigate to the `macor/omg_gen`, and run the following command:

```bash
cargo run -- generate your-protocol.AnB -o output.rs
```

This generates the protocol in `output.rs` based on the protocol from `your-protocol.AnB`.

To use the generated protocol, place that file in a Rust project with `omg` as a dependency, and include the protocol with `mod output;` where `output` refers to the name of the generated file.

For an example of how to use the generated file, see `omg_example`.
