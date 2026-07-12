use std::{
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
};

use tokens89_core::{charset, graphlink, tifile::TiFile, tokenize, VariableType};

fn collect_files(root: &Path, extension: &str, files: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(root).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            collect_files(&path, extension, files);
        } else if path
            .extension()
            .and_then(|value| value.to_str())
            .is_some_and(|value| value.eq_ignore_ascii_case(extension))
        {
            files.push(path);
        }
    }
}

fn pair_key(path: &Path) -> String {
    path.file_stem()
        .unwrap()
        .to_string_lossy()
        .rsplit('.')
        .next()
        .unwrap()
        .to_ascii_lowercase()
}

fn parallel_pairs() -> Vec<(PathBuf, PathBuf)> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../corpus/parallel/TI-89-master");
    let mut sources = Vec::new();
    collect_files(&root, "txt", &mut sources);

    let mut compiled = Vec::new();
    collect_files(&root, "89p", &mut compiled);
    collect_files(&root, "89f", &mut compiled);
    let compiled = compiled
        .into_iter()
        .map(|path| (pair_key(&path), path))
        .collect::<BTreeMap<_, _>>();

    sources.sort();
    sources
        .into_iter()
        .map(|source| {
            let key = pair_key(&source);
            let binary = compiled
                .get(&key)
                .unwrap_or_else(|| panic!("no compiled file matches {}", source.display()))
                .clone();
            (source, binary)
        })
        .collect()
}

fn is_tokenized(file: &TiFile) -> bool {
    !matches!(file.kind, VariableType::Program | VariableType::Function)
        || (file.payload.len() >= 4 && file.payload[file.payload.len() - 2] & 8 == 0)
}

fn semantic_payload(mut payload: Vec<u8>) -> Vec<u8> {
    if payload.len() >= 10 && payload.last() == Some(&0xdc) {
        // Two AMS-maintained wrapper bytes vary across transfers.
        let body_size = payload.len() - 8;
        let reserved = payload.len() - 3;
        payload[body_size] = 0;
        payload[reserved] = 0;
    }
    // Branch displacements are regenerated and may select a different but
    // equivalent jump target convention. The instruction stream is compared
    // independently from those two-byte addresses.
    for index in 3..payload.len() {
        if payload[index] == 0xe4 && tokens89_core::token::has_displacement(payload[index - 1]) {
            payload[index - 3] = 0;
            payload[index - 2] = 0;
        }
    }
    payload
}

#[test]
fn graphlink_sources_and_compiled_files_match_in_both_directions() {
    let corpus = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../corpus");
    if !corpus.is_dir() {
        eprintln!("skipping parallel-corpus validation: corpus directory is absent");
        return;
    }
    let pairs = parallel_pairs();
    assert_eq!(pairs.len(), 38, "parallel corpus file count changed");

    let mut failures = Vec::new();
    for (source_path, binary_path) in pairs {
        let source = match graphlink::parse(&fs::read(&source_path).unwrap()) {
            Ok(source) => source,
            Err(error) => {
                failures.push(format!(
                    "{}: source parse failed: {error}",
                    source_path.display()
                ));
                continue;
            }
        };
        let binary = match TiFile::parse(&fs::read(&binary_path).unwrap()) {
            Ok(binary) => binary,
            Err(error) => {
                failures.push(format!(
                    "{}: binary parse failed: {error}",
                    binary_path.display()
                ));
                continue;
            }
        };
        assert_eq!(binary.to_bytes().unwrap(), fs::read(&binary_path).unwrap());
        if source.name != binary.name {
            failures.push(format!(
                "{} and {} name mismatch: {:?} != {:?}",
                source_path.display(),
                binary_path.display(),
                source.name,
                binary.name
            ));
        }
        if !matches!(binary.kind, VariableType::Program | VariableType::Function) {
            failures.push(format!(
                "{} has unexpected type {:?}",
                binary_path.display(),
                binary.kind
            ));
            continue;
        }

        let tokenized = is_tokenized(&binary);
        let unicode_source = charset::decode_source(&source.source);
        match tokens89_core::encode_source(
            &unicode_source,
            &binary.folder,
            &binary.name,
            binary.kind,
            tokenized,
        )
        .and_then(|bytes| TiFile::parse(&bytes))
        {
            Ok(generated)
                if generated.folder == binary.folder
                    && generated.name == binary.name
                    && generated.kind == binary.kind
                    && semantic_payload(generated.payload.clone())
                        == semantic_payload(binary.payload.clone()) => {}
            Ok(generated) => {
                let payload = generated.payload;
                failures.push(format!(
                "{} -> {}: tokenized payload differs at {:#x} ({} != {} bytes, {} differing positions); generated {:02x?}; corpus {:02x?}; first differences {:?}",
                source_path.display(), binary_path.display(),
                payload.iter().zip(&binary.payload).position(|(a, b)| a != b).unwrap_or(payload.len().min(binary.payload.len())),
                payload.len(), binary.payload.len(),
                payload.iter().zip(&binary.payload).filter(|(a, b)| a != b).count(),
                &payload[payload.iter().zip(&binary.payload).position(|(a, b)| a != b).unwrap_or(0).saturating_sub(6)..(payload.iter().zip(&binary.payload).position(|(a, b)| a != b).unwrap_or(0) + 10).min(payload.len())],
                &binary.payload[payload.iter().zip(&binary.payload).position(|(a, b)| a != b).unwrap_or(0).saturating_sub(6)..(payload.iter().zip(&binary.payload).position(|(a, b)| a != b).unwrap_or(0) + 10).min(binary.payload.len())],
                payload.iter().zip(&binary.payload).enumerate().filter(|(_, (a, b))| a != b).take(20).map(|(index, (a, b))| (index, *a, *b)).collect::<Vec<_>>()
                ));
            }
            Err(error) => failures.push(format!(
                "{} -> {}: encoding failed: {error}",
                source_path.display(),
                binary_path.display()
            )),
        }

        match tokens89_core::decode_file(&fs::read(&binary_path).unwrap()) {
            Ok(decoded) => {
                let decoded =
                    charset::encode_source(&tokens89_core::normalize_line_endings(&decoded))
                        .unwrap();
                match tokenize::tokenize(&decoded, binary.kind, tokenized) {
                    Ok(payload)
                        if semantic_payload(payload.clone())
                            == semantic_payload(binary.payload.clone()) => {}
                    Ok(payload) => {
                        let expected = semantic_payload(binary.payload.clone());
                        let actual = semantic_payload(payload);
                        let first = actual
                            .iter()
                            .zip(&expected)
                            .position(|(a, b)| a != b)
                            .unwrap_or(actual.len().min(expected.len()));
                        failures.push(format!(
                        "{} -> {}: decoded source is not semantically equivalent at payload {first:#x}; generated {:02x?}; corpus {:02x?}",
                        binary_path.display(), source_path.display(),
                        &actual[first.saturating_sub(8)..(first + 12).min(actual.len())],
                        &expected[first.saturating_sub(8)..(first + 12).min(expected.len())]
                    ));
                    }
                    Err(error) => failures.push(format!(
                        "{} -> {}: decoded source cannot be re-tokenized: {error}",
                        binary_path.display(),
                        source_path.display()
                    )),
                }
            }
            Err(error) => failures.push(format!(
                "{} -> {}: detokenization failed: {error}",
                binary_path.display(),
                source_path.display()
            )),
        }
    }

    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
