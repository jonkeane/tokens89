#![no_main]

use libfuzzer_sys::fuzz_target;
use tokens89_core::{charset, tokenize, VariableType};

const KINDS: [VariableType; 7] = [
    VariableType::Expression,
    VariableType::List,
    VariableType::Matrix,
    VariableType::Text,
    VariableType::String,
    VariableType::Program,
    VariableType::Function,
];

fuzz_target!(|data: &[u8]| {
    let Some((&selector, source)) = data.split_first() else {
        return;
    };
    let text = String::from_utf8_lossy(source);
    if let Ok(calculator) = charset::encode_source(&text) {
        let kind = KINDS[selector as usize % KINDS.len()];
        let _ = tokenize::tokenize(&calculator, kind, selector & 0x80 == 0);
    }
});
