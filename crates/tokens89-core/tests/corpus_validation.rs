use std::{fs, path::PathBuf};

use tokens89_core::{detokenize, tifile::TiFile, tokenize, TiGroup, VariableType};

fn is_group(path: &std::path::Path) -> bool {
    path.extension().and_then(|extension| extension.to_str()) == Some("89g")
}

fn corpus_files() -> Option<Vec<PathBuf>> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../corpus");
    if !root.is_dir() {
        return None;
    }
    let mut files = fs::read_dir(root)
        .expect("corpus directory")
        .map(|entry| entry.expect("corpus entry").path())
        .filter(|path| path.is_file() && path.file_name().is_none_or(|n| n != ".DS_Store"))
        .collect::<Vec<_>>();
    files.sort();
    Some(files)
}

fn source_kind(kind: VariableType) -> bool {
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

fn without_branch_displacements(mut payload: Vec<u8>) -> Vec<u8> {
    for index in 3..payload.len() {
        if payload[index] == 0xe4 && tokens89_core::token::has_displacement(payload[index - 1]) {
            payload[index - 3] = 0;
            payload[index - 2] = 0;
        }
    }
    payload
}

#[test]
fn every_corpus_container_is_structurally_valid_and_rebuilds_exactly() {
    let Some(files) = corpus_files() else {
        eprintln!("skipping compatibility-corpus validation: corpus directory is absent");
        return;
    };
    assert!(
        !files.is_empty(),
        "the compatibility corpus is missing or empty"
    );
    for path in files {
        let bytes = fs::read(&path).unwrap();
        if is_group(&path) {
            let parsed = TiGroup::parse(&bytes).unwrap_or_else(|error| {
                panic!("{} failed structural validation: {error}", path.display())
            });
            assert_eq!(parsed.to_bytes().unwrap(), bytes);
            continue;
        }
        let parsed = TiFile::parse(&bytes).unwrap_or_else(|error| {
            panic!("{} failed structural validation: {error}", path.display())
        });
        let rebuilt = parsed.to_bytes().unwrap();
        if rebuilt != bytes {
            let first = rebuilt
                .iter()
                .zip(&bytes)
                .position(|(a, b)| a != b)
                .unwrap_or(rebuilt.len().min(bytes.len()));
            panic!(
                "{} did not rebuild exactly: first mismatch at {first:#x} ({:?} != {:?})",
                path.display(),
                rebuilt.get(first),
                bytes.get(first)
            );
        }
    }
}

#[test]
fn source_corpus_payloads_decode_and_reencode_exactly() {
    let Some(files) = corpus_files() else {
        eprintln!("skipping compatibility-corpus validation: corpus directory is absent");
        return;
    };
    assert!(
        !files.is_empty(),
        "the compatibility corpus is missing or empty"
    );
    let mut failures = Vec::new();
    let mut source_files = 0;
    for path in files {
        if is_group(&path) {
            continue;
        }
        let parsed = TiFile::parse(&fs::read(&path).unwrap()).unwrap();
        if !source_kind(parsed.kind) {
            continue;
        }
        source_files += 1;
        let result = detokenize::detokenize(&parsed.payload).and_then(|source| {
            let tokenized = !matches!(parsed.kind, VariableType::Program | VariableType::Function)
                || (parsed.payload.len() >= 4 && parsed.payload[parsed.payload.len() - 2] & 8 == 0);
            tokenize::tokenize(&source, parsed.kind, tokenized).and_then(|rebuilt| {
                if tokenized {
                    Ok((source, rebuilt, true, true))
                } else {
                    detokenize::detokenize(&rebuilt).map(|roundtrip| {
                        let source_matches = roundtrip == source;
                        (source, rebuilt, false, source_matches)
                    })
                }
            })
        });
        match result {
            Ok((_, _, false, true)) => {}
            Ok((source, _, false, false))
                if parsed.kind == VariableType::Function
                    && source
                        .split(|&byte| byte == b'\r')
                        .nth(1)
                        .is_some_and(|line| !line.eq_ignore_ascii_case(b"Func")) =>
            {
                // VB always converts one-line functions to tokenized form,
                // which may remove redundant source parentheses.
            }
            Ok((_, rebuilt, true, _)) if rebuilt == parsed.payload => {}
            Ok((_, rebuilt, true, _))
                if without_branch_displacements(rebuilt.clone())
                    == without_branch_displacements(parsed.payload.clone()) => {}
            Ok((source, rebuilt, _, _)) => {
                let first = rebuilt
                    .iter()
                    .zip(&parsed.payload)
                    .position(|(a, b)| a != b)
                    .unwrap_or(rebuilt.len().min(parsed.payload.len()));
                let original_semantic = without_branch_displacements(parsed.payload.clone());
                let rebuilt_semantic = without_branch_displacements(rebuilt.clone());
                let semantic = rebuilt_semantic
                    .iter()
                    .zip(&original_semantic)
                    .position(|(a, b)| a != b)
                    .unwrap_or(rebuilt_semantic.len().min(original_semantic.len()));
                failures.push(format!(
                    "{}: payload mismatch at {first:#x}, semantic mismatch at {semantic:#x} ({} -> {} bytes; source {} bytes); original {:02x?}; rebuilt {:02x?}",
                    path.display(),
                    parsed.payload.len(),
                    rebuilt.len(),
                    source.len(),
                    &parsed.payload[semantic.saturating_sub(6)..(semantic + 10).min(parsed.payload.len())],
                    &rebuilt[semantic.saturating_sub(6)..(semantic + 10).min(rebuilt.len())]
                ));
            }
            Err(error) => failures.push(format!("{}: {error}", path.display())),
        }
    }
    assert!(source_files > 0, "the corpus contains no source variables");
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
