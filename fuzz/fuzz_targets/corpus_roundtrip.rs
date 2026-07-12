#![no_main]

use libfuzzer_sys::fuzz_target;
use tokens89_core::{detokenize, tifile::TiFile, tokenize, VariableType};

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

fuzz_target!(|data: &[u8]| {
    let Ok(file) = TiFile::parse(data) else {
        return;
    };
    if !is_source(file.kind) {
        return;
    }
    let Ok(source) = detokenize::detokenize(&file.payload) else {
        return;
    };
    let tokenized = !matches!(file.kind, VariableType::Program | VariableType::Function)
        || (file.payload.len() >= 4 && file.payload[file.payload.len() - 2] & 8 == 0);
    if let Ok(rebuilt) = tokenize::tokenize(&source, file.kind, tokenized) {
        let decoded = detokenize::detokenize(&rebuilt)
            .expect("tokenizer output must always be accepted by the detokenizer");
        assert_eq!(decoded, source);
    }
});
