use std::{fs, io, path::Path};

const TARGETS: [&str; 7] = [
    "tifile",
    "group",
    "detokenize",
    "graphlink",
    "charset",
    "tokenize",
    "corpus_roundtrip",
];

fn collect_files(root: &Path, files: &mut Vec<std::path::PathBuf>) -> io::Result<()> {
    for entry in fs::read_dir(root)? {
        let path = entry?.path();
        if path.is_dir() {
            collect_files(&path, files)?;
        } else if path.file_name().and_then(|name| name.to_str()) != Some(".DS_Store") {
            let extension = path
                .extension()
                .and_then(|extension| extension.to_str())
                .unwrap_or_default();
            if (extension.len() == 3 && extension.starts_with("89"))
                || extension.eq_ignore_ascii_case("txt")
            {
                files.push(path);
            }
        }
    }
    Ok(())
}

fn main() -> io::Result<()> {
    let workspace = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let mut files = Vec::new();
    collect_files(&workspace.join("corpus"), &mut files)?;
    files.sort();

    let output = workspace.join("target/fuzz-corpus");
    fs::create_dir_all(&output)?;
    for target in TARGETS {
        let target_dir = output.join(target);
        fs::create_dir_all(&target_dir)?;
        for entry in fs::read_dir(&target_dir)? {
            let path = entry?.path();
            if path
                .file_name()
                .and_then(|name| name.to_str())
                .is_some_and(|name| name.starts_with("seed-"))
            {
                fs::remove_file(path)?;
            }
        }
        let target_files = files.iter().filter(|source| {
            let extension = source
                .extension()
                .and_then(|extension| extension.to_str())
                .unwrap_or_default();
            let group = extension.eq_ignore_ascii_case("89g");
            let variable = extension.len() == 3 && extension.starts_with("89") && !group;
            let graphlink = extension.eq_ignore_ascii_case("txt");
            match target {
                "tifile" | "corpus_roundtrip" => variable,
                "group" => group,
                "graphlink" => graphlink,
                "detokenize" | "charset" | "tokenize" => variable || group || graphlink,
                _ => false,
            }
        });
        let mut copied = 0;
        for (index, source) in target_files.enumerate() {
            let extension = source
                .extension()
                .and_then(|extension| extension.to_str())
                .unwrap_or("seed");
            fs::copy(
                source,
                target_dir.join(format!("seed-{index:04}.{extension}")),
            )?;
            copied += 1;
        }
        eprintln!("copied {copied} recursive corpus seeds to {target}");
    }
    Ok(())
}
