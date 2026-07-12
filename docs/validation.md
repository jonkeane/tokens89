# Corpus validation

Phase 11 validation is automated by `crates/tokens89-core/tests/corpus_validation.rs`.
The current corpus contains 33 independently created TI-89 files:

- all 33 headers, declared lengths, checksums, metadata, and payloads parse;
- all 33 containers parse and serialize byte-for-byte;
- all 26 source-bearing program, function, and text files decode and recode;
- all seven ZIP variables are structurally verified and rejected by `decode` with
  an explicit unsupported-variable-type error;
- tokenized streams recode exactly, apart from branch displacements that are
  regenerated from the program structure and compared after normalization;
- untokenized program/function trailers are allowed to canonicalize, while their
  decoded source must remain exact.

Protected programs can contain deliberately invalid expression stacks. The
detokenizer represents an otherwise undecodable statement as `@tokens(hex)`.
The tokenizer accepts this escape only as a complete statement and restores the
bytes exactly. This notation is a lossless transport representation, not
TI-BASIC syntax.

Phase 12 has deterministic robustness coverage for arbitrary bounded TI files,
token streams, GraphLink inputs, and character data, plus every truncation of
every corpus file. The dependency-free mutational driver in
`crates/tokens89-core/examples/fuzz.rs` seeds from the corpus and checks
parse/write metadata properties. A one-million-iteration release-mode campaign
completed successfully after its findings were fixed. `cargo-fuzz` is not
installed in the current environment, so coverage-guided fuzzing remains an
additional release hardening task.

## Remaining external validation

The repository can establish structural, token, round-trip, and ecosystem
validation locally. AMS validation still requires loading representative output
in a TI-89 emulator or physical calculator; no emulator or calculator transport
is available in this workspace.
