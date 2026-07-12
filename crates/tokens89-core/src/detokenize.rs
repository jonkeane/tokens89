use crate::{number, token, Error, Result};
use std::cell::Cell;

const MAX_PARSE_DEPTH: usize = 64;

thread_local! {
    static PARSE_DEPTH: Cell<usize> = const { Cell::new(0) };
}

struct ParseDepthGuard<'a>(&'a Cell<usize>);

impl Drop for ParseDepthGuard<'_> {
    fn drop(&mut self) {
        self.0.set(self.0.get() - 1);
    }
}

fn is_legacy_text(p: &[u8]) -> bool {
    p.len() >= 10
        && p[p.len() - 1] == 0xdc
        && p[p.len() - 2] == 8
        && p[p.len() - 5] == 0xe5
        && p[p.len() - 6] == 0xe4
}
pub fn detokenize(payload: &[u8]) -> Result<Vec<u8>> {
    if payload.len() >= 4 && payload[..2] == [0, 1] && payload[payload.len() - 2..] == [0, 0xe0] {
        return Ok(payload[2..payload.len() - 2].to_vec());
    }
    if is_legacy_text(payload) {
        return Ok(payload[..payload.len() - 10].to_vec());
    }
    if payload.len() >= 5 && payload.last() == Some(&0xdc) && payload[payload.len() - 2] & 8 == 0 {
        return internal_bytes(&detokenize_program(payload)?);
    }
    let start = payload
        .iter()
        .position(|&b| b == token::END_STACK)
        .map(|x| x + 1)
        .unwrap_or(0);
    let (s, pos) = parse_back(payload, payload.len(), start)?;
    if pos != start {
        return Err(Error::MalformedDetail(format!(
            "{} unconsumed token bytes",
            pos.abs_diff(start)
        )));
    }
    internal_bytes(&s)
}

fn internal_bytes(s: &str) -> Result<Vec<u8>> {
    let mut out = Vec::new();
    for ch in s.chars() {
        if ch as u32 <= 255 {
            out.push(ch as u8);
        } else {
            out.extend(crate::charset::encode_source(&ch.to_string())?);
        }
    }
    Ok(out)
}

fn detokenize_program(data: &[u8]) -> Result<String> {
    if data.len() < 5 || data.last() != Some(&0xdc) {
        return Err(Error::Malformed("invalid program wrapper"));
    }
    let mut p = data.len() - 4;
    let mut args = Vec::new();
    while p > 0 && data[p - 1] != 0xe5 {
        let (arg, next) = parse_back(data, p, 0)?;
        if next >= p {
            return Err(Error::Malformed("program argument made no progress"));
        }
        args.push(arg);
        p = next;
    }
    if p == 0 {
        return Err(Error::Malformed("program argument list has no end tag"));
    }
    p -= 1;
    let mut out = format!("({})\r", args.join(","));
    while p > 0 {
        let t = data[p - 1];
        match t {
            0xe9 => p -= 1,
            0xe8 => {
                if p < 2 {
                    return Err(Error::Malformed("truncated line token"));
                }
                let indent = data[p - 2] as usize;
                out.push('\r');
                out.push_str(&" ".repeat(indent));
                p -= 2;
            }
            0xe7 => {
                if p < 2 {
                    return Err(Error::Malformed("truncated colon token"));
                }
                let indent = data[p - 2] as usize;
                out.push(':');
                out.push_str(&" ".repeat(indent));
                p -= 2;
            }
            0xe6 => {
                if p < 3 {
                    return Err(Error::Malformed("truncated comment token"));
                }
                let indent = data[p - 2] as usize;
                let end = p - 3;
                let mut start = end;
                while start > 0 && data[start - 1] != 0 {
                    start -= 1;
                }
                if start == 0 {
                    return Err(Error::Malformed("unterminated comment token"));
                }
                out.push_str(&" ".repeat(indent));
                out.push('\u{00a9}');
                out.extend(data[start..end].iter().map(|&b| b as char));
                p = start - 1;
            }
            0xe4 => {
                if p < 2 {
                    return Err(Error::Malformed("truncated instruction token"));
                }
                let code = data[p - 2];
                p -= 2;
                if crate::token::has_displacement(code) {
                    if p < 2 {
                        return Err(Error::Malformed("truncated branch displacement"));
                    }
                    p -= 2;
                }
                let name =
                    crate::token::instruction_name(code).ok_or(Error::UnsupportedToken(code))?;
                out.push_str(name);
                let arity = crate::token::instruction_arity(code);
                let mut values = Vec::new();
                match arity {
                    Some(n) => {
                        for _ in 0..n {
                            let (v, next) = parse_back(data, p, 0)?;
                            values.push(v);
                            p = next;
                        }
                    }
                    None => {
                        while p > 0 && data[p - 1] != 0xe5 {
                            let (v, next) = parse_back(data, p, 0)?;
                            values.push(v);
                            p = next;
                        }
                        if p == 0 {
                            return Err(Error::Malformed(
                                "variable instruction arguments have no end tag",
                            ));
                        }
                        p -= 1;
                    }
                }
                if !values.is_empty() {
                    out.push(' ');
                    out.push_str(&values.join(if code == 0x86 { "=" } else { "," }));
                }
                if matches!(code, 0x39 | 0x3b) {
                    out.push_str(" Then");
                }
            }
            0xe5 => p -= 1,
            _ => {
                match parse_back(data, p, 0) {
                    Ok((value, next)) => {
                        out.push_str(&value);
                        p = next;
                    }
                    Err(error) => {
                        // Some protected programs contain deliberately invalid
                        // expression stacks. Preserve such a statement in a
                        // reversible textual form instead of losing its bytes.
                        let Some(delimiter) = (1..p)
                            .rev()
                            .find(|&index| matches!(data[index], 0xe7 | 0xe8))
                        else {
                            return Err(error);
                        };
                        out.push_str("@tokens(");
                        for byte in &data[delimiter + 1..p] {
                            out.push_str(&format!("{byte:02x}"));
                        }
                        out.push(')');
                        p = delimiter + 1;
                    }
                }
            }
        }
    }
    Ok(out)
}
fn parse_back(data: &[u8], end: usize, start: usize) -> Result<(String, usize)> {
    PARSE_DEPTH.with(|depth| {
        if depth.get() >= MAX_PARSE_DEPTH {
            return Err(Error::Malformed("token nesting exceeds 256 levels"));
        }
        depth.set(depth.get() + 1);
        let _guard = ParseDepthGuard(depth);
        parse_back_inner(data, end, start)
    })
}

fn parse_back_inner(data: &[u8], end: usize, start: usize) -> Result<(String, usize)> {
    if end <= start {
        return Err(Error::Malformed("truncated token stream"));
    }
    let t = data[end - 1];
    if t == 0x1f || t == 0x20 {
        // 0x1f/0x20 are also used as non-numeric tokens in some streams.
        // Only parse as integer when the encoded length byte is plausible.
        if end >= 3 {
            let n = data[end - 2] as usize;
            if end >= n + 2 {
                return number::decode_integer(data, end);
            }
        }
    }
    if t == 0x23 && end >= 10 {
        if let Ok(v) = number::decode_float(data, end) {
            return Ok(v);
        }
    }
    if matches!(t, 0x21 | 0x22) && end >= 3 {
        let nlen = data[end - 2] as usize;
        if nlen > 0 && end >= nlen + 2 {
            if let Ok(v) = number::decode_fraction(data, end) {
                return Ok(v);
            }
        }
    }
    if t == 0xe3 {
        if end < 2 {
            return Err(Error::Malformed("truncated extended token"));
        }
        let sub = data[end - 2];
        if sub == 0x01 {
            let (value, p) = parse_back(data, end - 2, start)?;
            return Ok((format!("#{value}"), p));
        }
        if sub == 0x05 {
            let (right, p) = parse_back(data, end - 2, start)?;
            let (left, p) = parse_back(data, p, start)?;
            return Ok((format!("{left}\u{0012}{right}"), p));
        }
        if (0x1b..=0x25).contains(&sub) {
            let text = match sub {
                0x1b => "(",
                0x1c => ")",
                0x1d => "[",
                0x1e => "]",
                0x1f => "{",
                0x20 => "}",
                0x21 => ",",
                0x22 => ";",
                0x23 => "\u{009f}",
                0x24 => "'",
                _ => "\"",
            };
            return Ok((text.into(), end - 2));
        }
        if sub == 0x26 {
            let (right, p) = parse_back(data, end - 2, start)?;
            let (left, p) = parse_back(data, p, start)?;
            return Ok((format!("({left}\u{009f}{right})"), p));
        }
        let conversion = match sub {
            0x15 => Some("DD"),
            0x16 => Some("DMS"),
            0x17 => Some("Rect"),
            0x18 => Some("Polar"),
            0x19 => Some("Cylind"),
            0x1a => Some("Sphere"),
            0x2d => Some("Bin"),
            0x2e => Some("Dec"),
            0x2f => Some("Hex"),
            0x5f => Some("Grad"),
            0x60 => Some("Rad"),
            _ => None,
        };
        if let Some(name) = conversion {
            let (value, p) = parse_back(data, end - 2, start)?;
            return Ok((format!("{value}\u{0012}{name}"), p));
        }
        if matches!(sub, 0x2b | 0x2c) {
            let (decimal, p) = number::decode_integer(data, end - 2)?;
            let value: u128 = decimal
                .parse()
                .map_err(|_| Error::Malformed("invalid based integer"))?;
            return Ok((
                if sub == 0x2b {
                    format!("0b{value:b}")
                } else {
                    format!("0h{value:x}")
                },
                p,
            ));
        }
        if let Some(name) = token::extended_name(sub) {
            let mut p = end - 2;
            let mut args = Vec::new();
            if let Some(count) = token::extended_arity(sub) {
                for _ in 0..count {
                    let (arg, next) = parse_back(data, p, start)?;
                    args.push(arg);
                    p = next;
                }
            } else {
                while p > start && data[p - 1] != 0xe5 {
                    let (arg, next) = parse_back(data, p, start)?;
                    args.push(arg);
                    p = next;
                }
                if p <= start {
                    return Err(Error::Malformed("extended function has no end tag"));
                }
                p -= 1;
            }
            let separator = if sub == 0x35 { ";" } else { "," };
            return Ok((format!("{name}({})", args.join(separator)), p));
        }
        if sub == 0x14 {
            let (right, p) = parse_back(data, end - 2, start)?;
            let (left, p) = parse_back(data, p, start)?;
            return Ok((format!("{left}&{right}"), p));
        }
        return Err(Error::UnsupportedToken(sub));
    }
    if let Some(op) = token::binary_op(t) {
        let prec = token::precedence(op.trim());
        let first_end = end - 1;
        let (first, p) = parse_back(data, first_end, start)?;
        let second_end = p;
        let (second, p) = parse_back(data, second_end, start)?;
        // Some binary families are stored in source order. Since this parser
        // walks backward, their right operand is encountered first.
        let (left, left_end, right, right_end) = if token::binary_operands_are_in_source_order(t) {
            (second, second_end, first, first_end)
        } else {
            (first, first_end, second, second_end)
        };
        let lp = root_precedence(data, left_end);
        let rp = root_precedence(data, right_end);
        let right_associative = matches!(op, "^" | ".^");
        let associative = matches!(op, "+" | "*" | "and" | "or" | "xor");
        let same_associative_operator =
            associative && right_end > 0 && data.get(right_end - 1) == Some(&t);
        let left = if lp < prec || (lp == prec && right_associative) {
            format!("({left})")
        } else {
            left
        };
        let right = if rp < prec || (rp == prec && !same_associative_operator && !right_associative)
        {
            format!("({right})")
        } else {
            right
        };
        return Ok((format!("{left}{op}{right}"), p));
    }
    if let Some(name) = token::unary_name(t) {
        let (a, p) = parse_back(data, end - 1, start)?;
        return Ok((format!("{name}({a})"), p));
    }
    if matches!(t, 0x75..=0x78) {
        let (value, p) = parse_back(data, end - 1, start)?;
        let suffix = match t {
            0x75 => "\u{0099}",
            0x76 => "!",
            0x77 => "%",
            _ => "\u{0098}",
        };
        return Ok((format!("{value}{suffix}"), p));
    }
    if matches!(t, 0x7b..=0x7d) {
        let (matrix, p) = parse_back(data, end - 1, start)?;
        let inner = matrix
            .strip_prefix("[[")
            .and_then(|v| v.strip_suffix("]]"))
            .ok_or(Error::Malformed(
                "vector token is not followed by a row vector",
            ))?;
        let mut items = inner.split(',').map(str::to_owned).collect::<Vec<_>>();
        if items.len() < 2 {
            return Err(Error::Malformed("vector has too few elements"));
        }
        items[1] = format!("\u{009f}{}", items[1]);
        if t == 0x7d {
            if items.len() < 3 {
                return Err(Error::Malformed("spherical vector has too few elements"));
            }
            items[2] = format!("\u{009f}{}", items[2]);
        }
        return Ok((format!("[{}]", items.join(",")), p));
    }
    if t == 0xcd {
        let (degrees, p) = parse_back(data, end - 1, start)?;
        let (minutes, p) = parse_back(data, p, start)?;
        let (seconds, p) = parse_back(data, p, start)?;
        return Ok((format!("{degrees}\u{00b0}{minutes}'{seconds}\""), p));
    }
    if t == 0xf0 {
        return parse_back(data, end - 1, start);
    }
    if t == 0xd5 {
        let (value, mut p) = parse_back(data, end - 1, start)?;
        let mut indices = Vec::new();
        while p > start && data[p - 1] != 0xe5 {
            let (index, next) = parse_back(data, p, start)?;
            indices.push(index);
            p = next;
        }
        if p <= start {
            return Err(Error::Malformed("subscript has no end tag"));
        }
        return Ok((format!("{value}[{}]", indices.join(",")), p - 1));
    }
    if t == 0xd9 {
        let mut p = end - 1;
        let mut items = Vec::new();
        while p > start && data[p - 1] != 0xe5 {
            let (item, next) = parse_back(data, p, start)?;
            if next >= p {
                return Err(Error::Malformed("list token made no progress"));
            }
            items.push(item);
            p = next;
        }
        if p <= start || data[p - 1] != 0xe5 {
            return Err(Error::Malformed("unterminated list token"));
        }
        let rendered =
            if !items.is_empty() && items.iter().all(|v| v.starts_with('{') && v.ends_with('}')) {
                format!(
                    "[{}]",
                    items
                        .iter()
                        .map(|v| format!("[{}]", &v[1..v.len() - 1]))
                        .collect::<String>()
                )
            } else {
                format!("{{{}}}", items.join(","))
            };
        return Ok((rendered, p - 1));
    }
    if t == 0xbf {
        let (left, p) = parse_back(data, end - 1, start)?;
        let (right, p) = parse_back(data, p, start)?;
        return Ok((format!("ln({left})/ln({right})"), p));
    }
    if let Some(name) = token::builtin_name(t) {
        let mut p = end - 1;
        let mut args = Vec::new();
        if let Some(count) = token::builtin_arity(t) {
            for _ in 0..count {
                let (arg, next) = parse_back(data, p, start)?;
                args.push(arg);
                p = next;
            }
        } else {
            while p > start && data[p - 1] != 0xe5 {
                let (arg, next) = parse_back(data, p, start)?;
                if next >= p {
                    return Err(Error::Malformed("function argument made no progress"));
                }
                args.push(arg);
                p = next;
            }
            if p <= start || data[p - 1] != 0xe5 {
                return Err(Error::Malformed("function arguments have no end tag"));
            }
            p -= 1;
        }
        return Ok((format!("{name}({})", args.join(",")), p));
    }
    match t {
        0x1c => {
            if end < 2 {
                return Err(Error::Malformed("truncated system variable"));
            }
            let code = data[end - 2];
            let name = token::system_variable_name(code).ok_or(Error::UnsupportedToken(code))?;
            Ok((name.into(), end - 2))
        }
        0x7a => {
            let (a, p) = parse_back(data, end - 1, start)?;
            Ok((format!("−{a}"), p))
        }
        0xea => {
            let (a, p) = parse_back(data, end - 1, start)?;
            Ok((format!("±{a}"), p))
        }
        0x1d | 0x1e => {
            if end < 2 {
                return Err(Error::Malformed("truncated arbitrary constant"));
            }
            Ok((
                format!("@{}{}", if t == 0x1e { "n" } else { "" }, data[end - 2]),
                end - 2,
            ))
        }
        0x24 => Ok(("\u{008c}".into(), end - 1)),
        0x25 => Ok(("\u{0096}".into(), end - 1)),
        0x26 => Ok(("\u{0097}".into(), end - 1)),
        0x2b => Ok(("false".into(), end - 1)),
        0x2c => Ok(("true".into(), end - 1)),
        0x29 | 0x2a => Ok(("undef".into(), end - 1)),
        0x2e => Ok((String::new(), end - 1)),
        0x01..=0x0a => Ok((((t + 112) as char).to_string(), end - 1)),
        0x0b..=0x1b => Ok((((t + 86) as char).to_string(), end - 1)),
        0 => {
            let mut p = end - 1;
            while p > start && end - 1 - p < 17 && data[p - 1] != 0 {
                // Program statements are separated by an indentation byte and
                // E7/E8. An eight-character variable at the start of a statement
                // has no zero byte between its name and that delimiter, so do not
                // consume the delimiter as a ninth name character.
                if matches!(data[p - 1], 0xe7 | 0xe8) {
                    break;
                }
                p -= 1;
            }
            let preceded_by_zero = p > start && data[p - 1] == 0;
            let from = p;
            Ok((
                data[from..end - 1].iter().map(|&b| b as char).collect(),
                if preceded_by_zero { p - 1 } else { p },
            ))
        }
        0x2d => {
            if end < 3 || data[end - 2] != 0 {
                return Err(Error::Malformed("invalid string token"));
            }
            let mut p = end - 2;
            while p > start && data[p - 1] != 0 {
                p -= 1
            }
            if p == start {
                return Err(Error::Malformed("unterminated string token"));
            }
            let s: String = data[p..end - 2]
                .iter()
                .flat_map(|&b| {
                    if b == b'"' {
                        vec!['"', '"']
                    } else {
                        vec![b as char]
                    }
                })
                .collect();
            Ok((format!("\"{s}\""), p - 1))
        }
        0xda => {
            let (name, mut p) = parse_back(data, end - 1, start)?;
            let mut args = Vec::new();
            while p > start && data[p - 1] != 0xe5 {
                let (arg, next) = parse_back(data, p, start)?;
                args.push(arg);
                p = next;
            }
            if p <= start {
                return Err(Error::Malformed("function call has no end tag"));
            }
            Ok((format!("{name}({})", args.join(",")), p - 1))
        }
        0xf2 => {
            if end < 2 || data[end - 2] != 0xda {
                return Err(Error::Malformed("invalid prime function token"));
            }
            let (name, mut p) = parse_back(data, end - 2, start)?;
            let mut args = Vec::new();
            while p > start && data[p - 1] != 0xe5 {
                let (arg, next) = parse_back(data, p, start)?;
                args.push(arg);
                p = next;
            }
            if p <= start {
                return Err(Error::Malformed("prime function call has no end tag"));
            }
            Ok((format!("{name}'({})", args.join(",")), p - 1))
        }
        0x27 => Ok(("−\u{00be}".into(), end - 1)),
        0x28 => Ok(("\u{00be}".into(), end - 1)),
        0xef => {
            let (value, p) = parse_back(data, end - 1, start)?;
            Ok((format!("{value}'"), p))
        }
        _ => {
            let from = end.saturating_sub(8);
            let context = data[from..end]
                .iter()
                .map(|b| format!("{b:02x}"))
                .collect::<Vec<_>>()
                .join(" ");
            Err(Error::MalformedDetail(format!(
                "unsupported token 0x{t:02x} at payload offset 0x{:x}; trailing bytes: {context}",
                end - 1
            )))
        }
    }
}
fn root_precedence(data: &[u8], end: usize) -> u8 {
    if end == 0 {
        return 255;
    }
    token::binary_op(data[end - 1])
        .map(|op| token::precedence(op.trim()))
        .unwrap_or(255)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn detoken_addition() {
        assert_eq!(
            detokenize(&[0xe9, 1, 1, 0x1f, 2, 1, 0x1f, 0x8b]).unwrap(),
            b"1+2"
        );
    }
    #[test]
    fn detoken_store_uses_calculator_operand_order() {
        assert_eq!(detokenize(&[0xe9, 0x06, 0x08, 0x80]).unwrap(), b"v\x16x");
    }
    #[test]
    fn detoken_concatenation_uses_calculator_operand_order() {
        assert_eq!(
            detokenize(&[0xe9, 0, b'l', 0, 0x2d, 0x03, 0x14, 0xe3]).unwrap(),
            b"\"l\"&s"
        );
    }
    #[test]
    fn every_ordinary_binary_operator_detokenizes_in_vb_operand_order() {
        let source_order = [
            ("x+y", 0x8b),
            ("x.+y", 0x8c),
            ("x-y", 0x8d),
            ("x.-y", 0x8e),
            ("x*y", 0x8f),
            ("x.*y", 0x90),
            ("x/y", 0x91),
            ("x./y", 0x92),
            ("x±y", 0xeb),
        ];
        for (source, token) in source_order {
            assert_eq!(
                detokenize(&[0xe9, 0x08, 0x09, token]).unwrap(),
                crate::charset::encode_source(source).unwrap()
            );
        }

        let reverse_order = [
            ("x|y", 0x81),
            ("x xor y", 0x82),
            ("x or y", 0x83),
            ("x and y", 0x84),
            ("x<y", 0x85),
            ("x≤y", 0x86),
            ("x=y", 0x87),
            ("x≥y", 0x88),
            ("x>y", 0x89),
            ("x≠y", 0x8a),
            ("x^y", 0x93),
            ("x.^y", 0x94),
        ];
        for (source, token) in reverse_order {
            assert_eq!(
                detokenize(&[0xe9, 0x09, 0x08, token]).unwrap(),
                crate::charset::encode_source(source).unwrap()
            );
        }
    }

    #[test]
    fn detoken_extended_binary_forms_use_source_operand_order() {
        assert_eq!(
            detokenize(&[0xe9, 0x08, 0, b'_', b'm', 0, 0x05, 0xe3]).unwrap(),
            b"x\x12_m"
        );
        assert_eq!(
            detokenize(&[0xe9, 0x08, 0x09, 0x26, 0xe3]).unwrap(),
            b"(x\x9fy)"
        );
    }

    #[test]
    fn detoken_power_associativity_matches_the_vb_scan_direction() {
        assert_eq!(
            detokenize(&[0xe9, 0x0a, 0x09, 0x93, 0x08, 0x93]).unwrap(),
            b"x^y^z"
        );
        assert_eq!(
            detokenize(&[0xe9, 0x0a, 0x09, 0x08, 0x93, 0x93]).unwrap(),
            b"(x^y)^z"
        );
    }
    #[test]
    fn malformed_never_panics() {
        assert!(detokenize(&[0x1f]).is_err());
    }
    #[test]
    fn deeply_nested_tokens_return_an_error_instead_of_overflowing_the_stack() {
        let mut payload = vec![0xe9, 1, 1, 0x1f];
        payload.extend(std::iter::repeat_n(0x44, MAX_PARSE_DEPTH + 10));
        assert!(matches!(
            detokenize(&payload),
            Err(Error::Malformed(msg)) if msg.contains("token nesting exceeds")
        ));
    }

    #[test]
    fn tokenized_program_round_trip() {
        let source = b"()\rPrgm\r  ClrHome\r  Disp 1+2\rEndPrgm";
        let payload =
            crate::tokenize::tokenize(source, crate::VariableType::Program, true).unwrap();
        assert_eq!(detokenize(&payload).unwrap(), source);
    }
    #[test]
    fn eight_character_variable_does_not_consume_program_delimiter() {
        let source = b"()\rPrgm\r  foo12345:Lbl s\rEndPrgm";
        let payload =
            crate::tokenize::tokenize(source, crate::VariableType::Program, true).unwrap();
        assert_eq!(detokenize(&payload).unwrap(), source);
    }
    #[test]
    fn malformed_program_statement_uses_reversible_raw_escape() {
        let payload = [
            0xe9, 0x12, 0xe4, 0, 0xe8, 0x30, 0x30, 0x76, 0x65, 0x76, 0x65, 0x79, 0x7a, 0, 0xe8,
            0x19, 0xe4, 0xe5, 0, 0, 0, 0xdc,
        ];
        let source = detokenize(&payload).unwrap();
        assert!(String::from_utf8_lossy(&source).contains("@tokens(303076657665797a)"));
        assert_eq!(
            crate::tokenize::tokenize(&source, crate::VariableType::Program, true).unwrap(),
            payload
        );
    }
    #[test]
    fn precedence_matrix_and_comments_round_trip() {
        for source in [
            b"(1+2)*3".as_slice(),
            b"[[1,2][3,4]]",
            b"()\rPrgm\r  Disp (1+2)*3 \xa9note\r  \xa9only\rEndPrgm",
        ] {
            let kind = if source.starts_with(b"()") {
                crate::VariableType::Program
            } else {
                crate::VariableType::Expression
            };
            let payload = crate::tokenize::tokenize(source, kind, true).unwrap();
            assert_eq!(detokenize(&payload).unwrap(), source);
        }
    }

    #[test]
    fn mixed_multiplication_and_division_preserve_the_right_subtree() {
        let source = b"x*(sqrt(a)/sqrt(r))";
        let payload =
            crate::tokenize::tokenize(source, crate::VariableType::Expression, true).unwrap();
        let decoded = detokenize(&payload).unwrap();
        assert_eq!(decoded, b"x*(\xa8(a)/\xa8(r))");
        assert_eq!(
            crate::tokenize::tokenize(&decoded, crate::VariableType::Expression, true).unwrap(),
            payload
        );
    }
    #[test]
    fn every_short_byte_stream_returns_without_panicking() {
        for a in 0..=255u8 {
            let _ = detokenize(&[a]);
            for b in 0..=255u8 {
                let _ = detokenize(&[a, b]);
            }
        }
    }
    #[test]
    fn every_mapped_expression_token_has_a_valid_context() {
        let atom = [1, 1, 0x1f];
        for token in 0..=255u8 {
            if crate::token::unary_name(token).is_some() {
                let mut p = vec![0xe9];
                p.extend(atom);
                p.push(token);
                assert!(detokenize(&p).is_ok(), "unary {token:02x}");
            }
            if crate::token::builtin_name(token).is_some() {
                let mut p = vec![0xe9];
                if crate::token::builtin_arity(token).is_none() {
                    p.push(0xe5);
                }
                for _ in 0..crate::token::builtin_arity(token).unwrap_or(1) {
                    p.extend(atom);
                }
                p.push(token);
                assert!(detokenize(&p).is_ok(), "builtin {token:02x}");
            }
            if crate::token::binary_op(token).is_some() {
                let mut p = vec![0xe9];
                p.extend(atom);
                p.extend(atom);
                p.push(token);
                assert!(detokenize(&p).is_ok(), "binary {token:02x}");
            }
        }
        for code in 1..=0x71 {
            if crate::token::system_variable_name(code).is_some() {
                assert!(detokenize(&[0xe9, code, 0x1c]).is_ok(), "system {code:02x}");
            }
        }
        for sub in 1..=0x5d {
            if crate::token::extended_name(sub).is_some() {
                let mut p = vec![0xe9];
                if crate::token::extended_arity(sub).is_none() {
                    p.push(0xe5);
                }
                for _ in 0..crate::token::extended_arity(sub).unwrap_or(1) {
                    p.extend(atom);
                }
                p.extend([sub, 0xe3]);
                assert!(detokenize(&p).is_ok(), "extended {sub:02x}");
            }
        }
    }
    #[test]
    fn legacy_mrowadd_system_zoom_and_xlog_spellings_are_preserved() {
        let atom = [1, 1, 0x1f];
        let mut mrow = vec![0xe9, 0xe5];
        mrow.extend(atom);
        mrow.push(0xbb);
        assert_eq!(detokenize(&mrow).unwrap(), b"nInt(1)");
        let mut xlog = vec![0xe9];
        xlog.extend(atom);
        xlog.extend(atom);
        xlog.push(0xbf);
        assert_eq!(detokenize(&xlog).unwrap(), b"ln(1)/ln(1)");
        let a = crate::charset::encode_source("zpltstrt").unwrap();
        let payload = crate::tokenize::tokenize(&a, crate::VariableType::Expression, true).unwrap();
        assert_eq!(payload, [0xe9, 0x56, 0x1c]);
        assert_eq!(detokenize(&payload).unwrap(), b"zpltstep");
    }
    #[test]
    fn raw_extended_delimiters_are_defined() {
        let expected = ["(", ")", "[", "]", "{", "}", ",", ";", "∠", "'", "\""];
        for (sub, text) in (0x1b..=0x25).zip(expected) {
            let raw = detokenize(&[0xe9, sub, 0xe3]).unwrap();
            assert_eq!(crate::charset::decode_source(&raw), text);
        }
    }
    #[test]
    fn parenthesized_angle_extended_token_is_defined() {
        let atom = [1, 1, 0x1f];
        let mut payload = vec![0xe9];
        payload.extend(atom);
        payload.extend(atom);
        payload.extend([0x26, 0xe3]);
        assert_eq!(
            crate::charset::decode_source(&detokenize(&payload).unwrap()),
            "(1∠1)"
        );
    }
    #[test]
    fn malformed_extended_and_prime_tokens_are_rejected() {
        assert!(matches!(
            detokenize(&[0xe9, 0xff, 0xe3]),
            Err(Error::UnsupportedToken(0xff))
        ));
        assert!(matches!(
            detokenize(&[0xe9, 0xf2]),
            Err(Error::Malformed(msg)) if msg.contains("invalid prime function token")
        ));
    }
    #[test]
    fn malformed_composite_tokens_report_specific_errors() {
        assert!(matches!(
            detokenize(&[0xe9, 0x01, 0x01, 0x1f, 0xd9]),
            Err(Error::Malformed(msg)) if msg.contains("unterminated list token")
        ));

        assert!(matches!(
            detokenize(&[0xe9, 0x01, 0x01, 0x1f, 0x01, 0x01, 0x1f, 0xd5]),
            Err(Error::Malformed(msg)) if msg.contains("subscript has no end tag")
        ));

        assert!(matches!(
            detokenize(&[0xe9, 0x01, 0x01, 0x1f, 0xd7]),
            Err(Error::Malformed(msg)) if msg.contains("function arguments have no end tag")
        ));
    }
    #[test]
    fn truncated_system_variable_token_is_rejected() {
        assert!(matches!(
            detokenize(&[0xe9, 0x1c]),
            Err(Error::UnsupportedToken(0xe9))
        ));
    }
    #[test]
    fn every_instruction_token_detokenizes_in_a_program() {
        let atom = [1, 1, 0x1f];
        for code in 1..=0x9b {
            if crate::token::instruction_name(code).is_none() {
                continue;
            }
            let mut segment = Vec::new();
            match crate::token::instruction_arity(code) {
                Some(n) => {
                    for _ in 0..n {
                        segment.extend(atom)
                    }
                }
                None => segment.push(0xe5),
            }
            if crate::token::has_displacement(code) {
                segment.extend([0, 0]);
            }
            segment.extend([code, 0xe4]);
            let mut payload = vec![0xe9];
            payload.extend(segment);
            payload.extend([0xe5, 0, 0, 0, 0xdc]);
            assert!(detokenize(&payload).is_ok(), "instruction {code:02x}");
        }
    }
}
