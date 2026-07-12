use crate::{Error, Result};

// TI-GraphLink's legacy character set largely follows Windows-1252 above 0x9f,
// with calculator symbols occupying 0x80..0x9f and selected later positions.
const SYMBOLS: &[(u8, char, &str)] = &[
    (14, '🔒', "lock"),
    (15, '✓', "check"),
    (16, '■', "block"),
    (17, '⇤', "from"),
    (18, '⇥', "to"),
    (19, '⇧', "up"),
    (20, '⇩', "down"),
    (21, '←', "leftarrow"),
    (22, '→', "->"),
    (23, '↑', "uparrow"),
    (24, '↓', "downarrow"),
    (25, '◁', "left"),
    (26, '▷', "right"),
    (27, '⎇', "shift"),
    (28, '∪', "union"),
    (29, '∩', "intersect"),
    (30, '⊂', "subset"),
    (31, '∈', "element"),
    (127, '◇', "option"),
    (128, 'α', "alpha"),
    (129, 'β', "beta"),
    (130, 'Γ', "Gamma"),
    (131, 'γ', "gamma"),
    (132, 'Δ', "Delta"),
    (133, 'δ', "delta"),
    (134, 'ε', "epsilon"),
    (135, 'ζ', "zeta"),
    (136, 'θ', "theta"),
    (137, 'λ', "lambda"),
    (138, 'ξ', "xi"),
    (139, 'Π', "Pi"),
    (140, 'π', "pi"),
    (141, 'ρ', "rho"),
    (142, 'Σ', "Sigma"),
    (143, 'σ', "sigma"),
    (144, 'τ', "tau"),
    (145, 'φ', "phi"),
    (146, 'ψ', "psi"),
    (147, 'Ω', "Omega"),
    (148, 'ω', "omega"),
    (149, 'ᴇ', "ee"),
    (150, 'ℯ', "e"),
    (151, 'ⅈ', "i"),
    (152, 'ʳ', "r"),
    (153, 'ᵀ', "t"),
    (154, 'ẋ', "xmean"),
    (155, 'ẏ', "ymean"),
    (156, '≤', "<="),
    (157, '≠', "!="),
    (158, '≥', ">="),
    (159, '∠', "/_"),
    (160, '…', "..."),
    (168, '√', "root"),
    (169, '©', "(C)"),
    (173, '−', "(-)"),
    (175, '⁻', "^-"),
    (178, '²', "^2"),
    (179, '³', "^3"),
    (180, '⁻', "^-1"),
    (181, 'μ', "mu"),
    (184, 'ˣ', "^x"),
    (188, '∂', "diff"),
    (189, '∫', "integral"),
    (190, '∞', "infinity"),
];

pub fn encode_source(input: &str) -> Result<Vec<u8>> {
    let mut out = Vec::with_capacity(input.len());
    let mut chars = input.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '⁻' && chars.peek() == Some(&'¹') {
            chars.next();
            out.push(180);
            continue;
        }
        if ch as u32 <= 0x7f {
            out.push(ch as u8);
            continue;
        }
        if let Some((byte, _, _)) = SYMBOLS.iter().find(|(_, c, _)| *c == ch) {
            out.push(*byte);
            continue;
        }
        if (0xa0..=0xff).contains(&(ch as u32)) {
            out.push(ch as u8);
            continue;
        }
        let byte = match ch {
            '¢' => 0xa2,
            '£' => 0xa3,
            '¥' => 0xa5,
            '§' => 0xa7,
            '®' => 0xae,
            '°' => 0xb0,
            '±' => 0xb1,
            '·' => 0xb7,
            'À' => 0xc0,
            'Á' => 0xc1,
            'Â' => 0xc2,
            'Ã' => 0xc3,
            'Ä' => 0xc4,
            'Å' => 0xc5,
            'Æ' => 0xc6,
            'Ç' => 0xc7,
            'È' => 0xc8,
            'É' => 0xc9,
            'Ê' => 0xca,
            'Ë' => 0xcb,
            'Ì' => 0xcc,
            'Í' => 0xcd,
            'Î' => 0xce,
            'Ï' => 0xcf,
            'Ð' => 0xd0,
            'Ñ' => 0xd1,
            'Ò' => 0xd2,
            'Ó' => 0xd3,
            'Ô' => 0xd4,
            'Õ' => 0xd5,
            'Ö' => 0xd6,
            '×' => 0xd7,
            'Ø' => 0xd8,
            'Ù' => 0xd9,
            'Ú' => 0xda,
            'Û' => 0xdb,
            'Ü' => 0xdc,
            'Ý' => 0xdd,
            'Þ' => 0xde,
            'ß' => 0xdf,
            'à' => 0xe0,
            'á' => 0xe1,
            'â' => 0xe2,
            'ã' => 0xe3,
            'ä' => 0xe4,
            'å' => 0xe5,
            'æ' => 0xe6,
            'ç' => 0xe7,
            'è' => 0xe8,
            'é' => 0xe9,
            'ê' => 0xea,
            'ë' => 0xeb,
            'ì' => 0xec,
            'í' => 0xed,
            'î' => 0xee,
            'ï' => 0xef,
            'ð' => 0xf0,
            'ñ' => 0xf1,
            'ò' => 0xf2,
            'ó' => 0xf3,
            'ô' => 0xf4,
            'õ' => 0xf5,
            'ö' => 0xf6,
            '÷' => 0xf7,
            'ø' => 0xf8,
            'ù' => 0xf9,
            'ú' => 0xfa,
            'û' => 0xfb,
            'ü' => 0xfc,
            'ý' => 0xfd,
            'þ' => 0xfe,
            'ÿ' => 0xff,
            _ => return Err(Error::UnsupportedCharacter(ch)),
        };
        out.push(byte);
    }
    Ok(out)
}

pub fn decode_source(input: &[u8]) -> String {
    input
        .iter()
        .map(|&b| {
            if b == 180 {
                return '⁼';
            }
            SYMBOLS
                .iter()
                .find(|(byte, _, _)| *byte == b)
                .map(|(_, c, _)| *c)
                .unwrap_or_else(|| char::from_u32(b as u32).unwrap_or('\u{fffd}'))
        })
        .collect::<String>()
        .replace('⁼', "⁻¹")
}

pub fn graphlink_escape(byte: u8) -> Option<&'static str> {
    SYMBOLS
        .iter()
        .find(|(b, _, _)| *b == byte)
        .map(|(_, _, e)| *e)
        .or_else(|| {
            graphlink_extra()
                .iter()
                .find(|(b, _)| *b == byte)
                .map(|(_, e)| *e)
        })
}

pub fn graphlink_unescape(name: &str) -> Option<u8> {
    SYMBOLS
        .iter()
        .find(|(_, _, e)| *e == name)
        .map(|(b, _, _)| *b)
        .or_else(|| {
            graphlink_extra()
                .iter()
                .find(|(_, e)| *e == name)
                .map(|(b, _)| *b)
        })
}

fn graphlink_extra() -> &'static [(u8, &'static str)] {
    &[
        (0x7e, "lnot"),
        (0xa1, "ud!"),
        (0xa2, "cent"),
        (0xa3, "pound"),
        (0xa4, "starbust"),
        (0xa5, "yen"),
        (0xa6, "split"),
        (0xa7, "section"),
        (0xaa, "a_"),
        (0xab, "<<"),
        (0xae, "(R)"),
        (0xb0, "o"),
        (0xb1, "^+"),
        (0xb6, "para"),
        (0xb7, "."),
        (0xb9, "^1"),
        (0xba, "o_"),
        (0xbb, ">>"),
        (0xbf, "ud?"),
        (0xc0, "A`"),
        (0xc1, "A'"),
        (0xc2, "A^"),
        (0xc3, "A~"),
        (0xc4, "A.."),
        (0xc5, "Ao"),
        (0xc6, "AE"),
        (0xc7, "C,"),
        (0xc8, "E`"),
        (0xc9, "E'"),
        (0xca, "E^"),
        (0xcb, "E.."),
        (0xcc, "I`"),
        (0xcd, "I'"),
        (0xce, "I^"),
        (0xcf, "I.."),
        (0xd0, "-D"),
        (0xd1, "N~"),
        (0xd2, "O`"),
        (0xd3, "O'"),
        (0xd4, "O^"),
        (0xd5, "O~"),
        (0xd6, "O.."),
        (0xd7, "x"),
        (0xd8, "O/"),
        (0xd9, "U`"),
        (0xda, "U'"),
        (0xdb, "U^"),
        (0xdc, "U.."),
        (0xdd, "Y'"),
        (0xde, "I>"),
        (0xdf, "ss"),
        (0xe0, "a`"),
        (0xe1, "a'"),
        (0xe2, "a^"),
        (0xe3, "a~"),
        (0xe4, "a.."),
        (0xe5, "ao"),
        (0xe6, "ae"),
        (0xe7, "c,"),
        (0xe8, "e`"),
        (0xe9, "e'"),
        (0xea, "e^"),
        (0xeb, "e.."),
        (0xec, "i`"),
        (0xed, "i'"),
        (0xee, "i^"),
        (0xef, "i.."),
        (0xf0, "-d"),
        (0xf1, "n~"),
        (0xf2, "o`"),
        (0xf3, "o'"),
        (0xf4, "o^"),
        (0xf5, "o~"),
        (0xf6, "o.."),
        (0xf7, "/"),
        (0xf8, "o/"),
        (0xf9, "u`"),
        (0xfa, "u'"),
        (0xfb, "u^"),
        (0xfc, "u.."),
        (0xfd, "y'"),
        (0xfe, "i>"),
        (0xff, "y.."),
    ]
}

pub fn lowercase_byte(byte: u8) -> u8 {
    match byte {
        b'A'..=b'Z' => byte + 32,
        0xc0..=0xd6 | 0xd8..=0xde => byte + 32,
        _ => byte,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn greek_and_aliases_round_trip() {
        let b = encode_source("πθΣ≤").unwrap();
        assert_eq!(b, [140, 136, 142, 156]);
        assert_eq!(decode_source(&b), "πθΣ≤");
    }
    #[test]
    fn rejects_emoji() {
        assert!(matches!(
            encode_source("🙂"),
            Err(Error::UnsupportedCharacter(_))
        ));
    }
    #[test]
    fn lowercase_89() {
        assert_eq!(lowercase_byte(b'Q'), b'q');
        assert_eq!(lowercase_byte(0xc4), 0xe4);
    }
    #[test]
    fn all_graphlink_high_bytes_round_trip() {
        for byte in 0xc0..=0xff {
            let escape = graphlink_escape(byte).unwrap();
            assert_eq!(graphlink_unescape(escape), Some(byte));
        }
    }
    #[test]
    fn every_non_null_calculator_byte_round_trips_through_unicode() {
        for byte in 1..=u8::MAX {
            let decoded = decode_source(&[byte]);
            assert_eq!(
                encode_source(&decoded).unwrap(),
                [byte],
                "byte 0x{byte:02x}"
            );
        }
    }
    #[test]
    fn graphlink_option_character_is_not_treated_as_ascii_del() {
        assert_eq!(graphlink_escape(127), Some("option"));
        assert_eq!(graphlink_unescape("option"), Some(127));
        assert_eq!(encode_source("◇").unwrap(), [127]);
    }
}
