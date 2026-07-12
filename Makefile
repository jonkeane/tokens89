.PHONY: check test fmt fuzz fuzz-seeds bench
check:
	cargo fmt --check
	cargo test --workspace
	cargo clippy --workspace --all-targets -- -D warnings
test:
	cargo test --workspace
fmt:
	cargo fmt
fuzz:
	cargo run --release -p tokens89-core --example fuzz -- 1000000
fuzz-seeds:
	cargo run -p tokens89-core --example prepare_fuzz_seeds
bench:
	cargo bench --workspace
