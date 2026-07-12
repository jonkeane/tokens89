use std::{fs, hint::black_box, path::PathBuf, time::Instant};

use tokens89_core::{detokenize, tifile::TiFile, tokenize, TiGroup, VariableType};

fn is_source(kind: VariableType) -> bool {
    matches!(
        kind,
        VariableType::Expression
            | VariableType::List
            | VariableType::Matrix
            | VariableType::Text
            | VariableType::String
            | VariableType::Program
            | VariableType::Function
    )
}

fn main() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../corpus");
    let corpus = fs::read_dir(root)
        .unwrap()
        .filter_map(|entry| {
            let path = entry.ok()?.path();
            (path.is_file() && path.file_name().is_some_and(|name| name != ".DS_Store"))
                .then(|| fs::read(path).ok())?
        })
        .collect::<Vec<_>>();
    assert!(!corpus.is_empty(), "benchmark corpus is empty");
    let rounds = 500;
    let started = Instant::now();
    let mut processed = 0usize;
    for _ in 0..rounds {
        for bytes in &corpus {
            processed += bytes.len();
            if let Ok(group) = TiGroup::parse(black_box(bytes)) {
                for entry in group.entries {
                    if !is_source(entry.kind) {
                        continue;
                    }
                    let source = detokenize::detokenize(black_box(&entry.payload)).unwrap();
                    let tokenized =
                        !matches!(entry.kind, VariableType::Program | VariableType::Function)
                            || (entry.payload.len() >= 4
                                && entry.payload[entry.payload.len() - 2] & 8 == 0);
                    black_box(tokenize::tokenize(&source, entry.kind, tokenized).unwrap());
                }
            } else {
                let file = TiFile::parse(black_box(bytes)).unwrap();
                if !is_source(file.kind) {
                    continue;
                }
                let source = detokenize::detokenize(black_box(&file.payload)).unwrap();
                let tokenized =
                    !matches!(file.kind, VariableType::Program | VariableType::Function)
                        || (file.payload.len() >= 4
                            && file.payload[file.payload.len() - 2] & 8 == 0);
                black_box(tokenize::tokenize(&source, file.kind, tokenized).unwrap());
            }
        }
    }
    let elapsed = started.elapsed();
    let mib = processed as f64 / (1024.0 * 1024.0);
    eprintln!(
        "corpus parse/decode/recode: {mib:.1} MiB in {:.2?} ({:.1} MiB/s)",
        elapsed,
        mib / elapsed.as_secs_f64()
    );
}
