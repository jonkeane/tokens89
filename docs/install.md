# Installation and migration

## Prebuilt archives

Tagged releases produce archives for macOS on Apple Silicon and Intel, and
Linux on ARM64 and x86-64. Extract the archive and place `tokens89` somewhere
on `PATH`, such as `/usr/local/bin` or `$HOME/.local/bin`. Verify the archive
against its adjacent `.sha256` file before installing it.

Shell completion files for Bash, Zsh, and Fish are included in each archive.
Install the appropriate file using the conventions of your shell.

## Build from source

```sh
cargo build --release -p tokens89-cli
cargo test --workspace
```

The executable is `target/release/tokens89`. A future crates.io publication can
also be installed with `cargo install tokens89-cli`.

For a crates.io release, publish `tokens89-core` first and then
`tokens89-cli`; Cargo verifies the CLI package against the published versioned
core dependency.

## Migrating from the OCX

The CLI replaces the main OCX entry points as follows:

| OCX operation | CLI command |
| --- | --- |
| `OpenTI` | `tokens89 decode FILE` |
| `SaveTI` | `tokens89 encode SOURCE` |
| `TIVar` / header inspection | `tokens89 inspect FILE` |
| token validation | `tokens89 verify FILE` |
| `Token` | `tokens89 tokenize SOURCE` |
| `DeToken` | `tokens89 detokenize TOKENS` |

Source files use UTF-8 and ordinary LF line endings at the CLI boundary. The
converter handles calculator character and line-ending conversion internally.
Existing output files are not overwritten unless `--force` is supplied.

Protected programs may decode a deliberately malformed statement as
`@tokens(hex)`. Preserve that complete line to reproduce the original token
bytes; it is a transport escape rather than executable TI-BASIC source.
