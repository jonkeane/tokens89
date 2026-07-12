# Command-line usage

Build with `cargo build --release -p tokens89-cli`. The executable is
`target/release/tokens89`.

```text
tokens89 encode SOURCE --output FILE --name NAME [--folder main] --type TYPE
tokens89 decode FILE [--output SOURCE]
tokens89 inspect FILE
tokens89 tokenize SOURCE [--output TOKENS] [--hex]
tokens89 detokenize TOKENS [--output SOURCE] [--hex]
tokens89 verify FILE
tokens89 group FILE... [--output GROUP.89g]
```

`encode` infers program, function, expression, or text type from an explicitly
named `.89p`, `.89f`, `.89e`, or `.89t` output. Otherwise `--type` is required.
Use `-` for standard input or output. Existing files require `--force`.
Binary output is refused when standard output is an interactive terminal;
redirect it or pass `--output`.

The core accepts LF, CRLF, and CR source lines and preserves line-ending bytes
inside quoted strings. Unicode calculator symbols such as `π`, `θ`, `Σ`, `∫`,
`√`, and `→` are converted at the boundary.

Tokenization converts source representation. It is not a full TI-BASIC parser
or syntax validator, so a tokenized program may still contain calculator-side
syntax or runtime errors.

## Groups

`group` combines one or more single-variable TI files into an `.89g` group.
The variables retain their calculator folder, name, type, and payload.
`inspect` and `verify` also accept group files.

```sh
tokens89 group game.89p scores.89l readme.89t --output game.89g
```

Without `--output`, the first input's extension is replaced with `.89g`.
