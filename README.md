# Tokens89

Tokens89 is a portable Rust library and command-line converter for TI-89,
TI-92 Plus, and Voyage 200 variable files. It can inspect and verify TI files,
decode calculator programs to readable source, and encode source back into
tokenized or plain calculator variables.

```sh
cargo build --release -p tokens89-cli
target/release/tokens89 decode program.89p --output program.bas
target/release/tokens89 encode program.bas --output program.89p \
  --name program --folder main --type program
```

See `docs/cli.md` for command details, `docs/validation.md` for compatibility
evidence, and `docs/install.md` for installation and migration guidance.

## Licensing and provenance

This is a Rust port of the [Tokens89 VB6](http://pengels.bplaced.net/index.php/tiedit/tokens89) implementation.

- Tokens89 OCX copyright © 2000–2003 Kevin Kofler.
- Later Tokens89 work copyright © 2013–2015 Peter Engels.
- The original notices are retained in `priorDocumentation/TiFiles.ctl` and `priorDocumentation/readme.txt`.

The port is distributed under the same GNU Lesser General Public License,
version 2.1 or later. The complete license is in `lgpl.txt`.

## Getting tokenized files onto a TI calculator (without TI Connect)
[tilp2](https://github.com/debrouxl/tilp_and_gfm/tree/master/tilp/trunk) which depends on the [tilibs](https://github.com/debrouxl/tilibs/) provides a CLI (and GUI too) that can interface with TI calculators of USB and link cables without TI Connect