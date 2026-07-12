use std::{fs, path::PathBuf};

use tokens89_core::TiGroup;

#[test]
fn corpus_groups_parse_and_rebuild_exactly() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../corpus");
    if !root.is_dir() {
        eprintln!("skipping group-corpus validation: corpus directory is absent");
        return;
    }
    let corpus = root.join("groups");
    let mut found = 0;
    for path in fs::read_dir(corpus)
        .unwrap()
        .map(|entry| entry.unwrap().path())
    {
        if path.extension().and_then(|extension| extension.to_str()) != Some("89g") {
            continue;
        }
        found += 1;
        let bytes = fs::read(&path).unwrap();
        let group = TiGroup::parse(&bytes)
            .unwrap_or_else(|error| panic!("failed to parse {}: {error}", path.display()));
        assert_eq!(
            group.to_bytes().unwrap(),
            bytes,
            "group rebuild differs for {}",
            path.display()
        );
    }
    assert!(found > 0, "no .89g files found in corpus");
}
