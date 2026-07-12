use std::{fs, path::PathBuf, time::Instant};

use tokens89_core::{charset, detokenize, graphlink, tifile::TiFile, tokenize, VariableType};

const SOURCE_KINDS: [VariableType; 7] = [
    VariableType::Expression,
    VariableType::List,
    VariableType::Matrix,
    VariableType::Text,
    VariableType::String,
    VariableType::Program,
    VariableType::Function,
];

fn is_source(kind: VariableType) -> bool {
    SOURCE_KINDS.contains(&kind)
}

fn is_tokenized(payload: &[u8], kind: VariableType) -> bool {
    !matches!(kind, VariableType::Program | VariableType::Function)
        || (payload.len() >= 4 && payload[payload.len() - 2] & 8 == 0)
}

struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        self.0
    }

    fn index(&mut self, len: usize) -> usize {
        if len == 0 {
            0
        } else {
            self.next() as usize % len
        }
    }
}

fn corpus() -> Vec<Vec<u8>> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../corpus");
    let mut seeds = fs::read_dir(root)
        .expect("read corpus")
        .filter_map(|entry| fs::read(entry.ok()?.path()).ok())
        .collect::<Vec<_>>();
    seeds.push(
        graphlink::write("fuzz", b"()\rPrgm\rDisp 1\rEndPrgm").expect("create GraphLink seed"),
    );
    seeds
}

fn mutate(seed: &[u8], rng: &mut Rng, iteration: u64) -> Vec<u8> {
    let mut data = if iteration.is_multiple_of(1024) || seed.len() <= 4096 {
        seed.to_vec()
    } else {
        let start = rng.index(seed.len() - 4096);
        seed[start..start + 4096].to_vec()
    };
    for _ in 0..=rng.index(8) {
        match rng.index(5) {
            0 if !data.is_empty() => {
                let at = rng.index(data.len());
                data[at] ^= 1 << rng.index(8);
            }
            1 if !data.is_empty() => {
                let at = rng.index(data.len());
                data[at] = rng.next() as u8;
            }
            2 if data.len() < 65_536 => {
                let at = rng.index(data.len() + 1);
                data.insert(at, rng.next() as u8);
            }
            3 if !data.is_empty() => {
                let at = rng.index(data.len());
                data.remove(at);
            }
            4 if !data.is_empty() => data.truncate(rng.index(data.len())),
            _ => {}
        }
    }
    data
}

fn exercise(input: &[u8]) {
    if let Ok(file) = TiFile::parse(input) {
        let rebuilt = file.to_bytes().expect("parsed TI file must serialize");
        let reparsed = TiFile::parse(&rebuilt).expect("serialized TI file must parse");
        assert_eq!(reparsed.folder, file.folder);
        assert_eq!(reparsed.name, file.name);
        assert_eq!(reparsed.kind, file.kind);
        assert_eq!(reparsed.payload, file.payload);
        if is_source(file.kind) {
            if let Ok(source) = detokenize::detokenize(&file.payload) {
                let _ =
                    tokenize::tokenize(&source, file.kind, is_tokenized(&file.payload, file.kind));
            }
        }
    }
    if let Some((&selector, source)) = input.split_first() {
        let kind = SOURCE_KINDS[selector as usize % SOURCE_KINDS.len()];
        let _ = tokenize::tokenize(source, kind, selector & 0x80 == 0);
    }
    let _ = detokenize::detokenize(input);
    let _ = graphlink::parse(input);
    let decoded = charset::decode_source(input);
    let _ = charset::encode_source(&decoded);
}

fn main() {
    let iterations = std::env::args()
        .nth(1)
        .map(|value| value.parse::<u64>().expect("iteration count"))
        .unwrap_or(1_000_000);
    let seeds = corpus();
    let mut rng = Rng(0x0899_2200_5eed);
    let started = Instant::now();
    for iteration in 0..iterations {
        let seed = &seeds[rng.index(seeds.len())];
        let input = mutate(seed, &mut rng, iteration);
        exercise(&input);
    }
    let elapsed = started.elapsed();
    eprintln!(
        "completed {iterations} fuzz iterations in {:.2?} ({:.0}/s)",
        elapsed,
        iterations as f64 / elapsed.as_secs_f64()
    );
}
