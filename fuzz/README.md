# Fuzzing

The seven `cargo-fuzz` targets cover TI containers, group containers,
token-stream detokenization, GraphLink parsing, character conversion, source
tokenization, and full decode/recode behavior.

Never pass `./corpus` directly as a libFuzzer corpus directory: libFuzzer may
write new samples into its corpus directory. Prepare a disposable seeded copy:

```sh
make fuzz-seeds
cargo fuzz run tifile target/fuzz-corpus/tifile
cargo fuzz run group target/fuzz-corpus/group
cargo fuzz run detokenize target/fuzz-corpus/detokenize
cargo fuzz run graphlink target/fuzz-corpus/graphlink
cargo fuzz run charset target/fuzz-corpus/charset
cargo fuzz run tokenize target/fuzz-corpus/tokenize
cargo fuzz run corpus_roundtrip target/fuzz-corpus/corpus_roundtrip
```

Crash artifacts are written under `fuzz/artifacts/`. Every confirmed finding
must receive an ordinary regression test before its artifact is removed.
