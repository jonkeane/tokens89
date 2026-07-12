use crate::{Error, Result};

pub fn encode_integer(text: &str) -> Result<Vec<u8>> {
    let negative = text.starts_with('-') || text.starts_with('−');
    let digits = if negative { &text[1..] } else { text };
    let (radix, d) = if let Some(v) = digits
        .strip_prefix("0b")
        .or_else(|| digits.strip_prefix("0B"))
    {
        (2, v)
    } else if let Some(v) = digits
        .strip_prefix("0h")
        .or_else(|| digits.strip_prefix("0H"))
    {
        (16, v)
    } else {
        (10, digits)
    };
    if d.is_empty() {
        return Err(Error::MalformedDetail(format!("invalid integer {text}")));
    }
    let mut value = d
        .bytes()
        .map(|byte| {
            let digit = match byte {
                b'0'..=b'9' => byte - b'0',
                b'a'..=b'f' => byte - b'a' + 10,
                b'A'..=b'F' => byte - b'A' + 10,
                _ => radix as u8,
            };
            (digit < radix as u8)
                .then_some(digit)
                .ok_or_else(|| Error::MalformedDetail(format!("invalid integer {text}")))
        })
        .collect::<Result<Vec<_>>>()?;
    while value.first() == Some(&0) {
        value.remove(0);
    }
    let mut bytes = Vec::new();
    while !value.is_empty() {
        let mut quotient = Vec::with_capacity(value.len());
        let mut carry = 0u16;
        for digit in value {
            let current = carry * radix as u16 + digit as u16;
            let next = (current / 256) as u8;
            carry = current % 256;
            if !quotient.is_empty() || next != 0 {
                quotient.push(next);
            }
        }
        bytes.push(carry as u8);
        value = quotient;
    }
    if bytes.len() > u8::MAX as usize {
        return Err(Error::Malformed("integer exceeds the AMS token length"));
    }
    bytes.push(bytes.len() as u8);
    bytes.push(if negative { 0x20 } else { 0x1f });
    Ok(bytes)
}

fn magnitude_to_decimal(bytes: &[u8]) -> String {
    let mut decimal = vec![0u8];
    for &byte in bytes.iter().rev() {
        let mut carry = byte as u16;
        for digit in &mut decimal {
            let value = *digit as u16 * 256 + carry;
            *digit = (value % 10) as u8;
            carry = value / 10;
        }
        while carry != 0 {
            decimal.push((carry % 10) as u8);
            carry /= 10;
        }
    }
    decimal
        .iter()
        .rev()
        .map(|digit| char::from(b'0' + *digit))
        .collect()
}

pub fn decode_integer(data: &[u8], end: usize) -> Result<(String, usize)> {
    if end < 2 {
        return Err(Error::Malformed("truncated integer"));
    }
    let n = data[end - 2] as usize;
    if end < n + 2 {
        return Err(Error::Malformed("invalid integer length"));
    }
    let value = magnitude_to_decimal(&data[end - n - 2..end - 2]);
    let sign = if data[end - 1] == 0x20 { "−" } else { "" };
    Ok((format!("{sign}{value}"), end - n - 2))
}

pub fn encode_fraction(numerator: u128, denominator: u128, negative: bool) -> Result<Vec<u8>> {
    encode_fraction_decimal(&numerator.to_string(), &denominator.to_string(), negative)
}

pub fn encode_fraction_decimal(
    numerator: &str,
    denominator: &str,
    negative: bool,
) -> Result<Vec<u8>> {
    if denominator.bytes().all(|byte| byte == b'0') {
        return Err(Error::Malformed("fraction denominator is zero"));
    }
    fn magnitude(value: &str) -> Result<Vec<u8>> {
        let encoded = encode_integer(value)?;
        let length = encoded[encoded.len() - 2] as usize;
        if length == 0 {
            Ok(vec![0])
        } else {
            Ok(encoded[..length].to_vec())
        }
    }
    let d = magnitude(denominator)?;
    let n = magnitude(numerator)?;
    let mut out = Vec::new();
    out.extend(&d);
    out.push(d.len() as u8);
    out.extend(&n);
    out.push(n.len() as u8);
    out.push(if negative { 0x22 } else { 0x21 });
    Ok(out)
}
pub fn decode_fraction(data: &[u8], end: usize) -> Result<(String, usize)> {
    if end < 3 {
        return Err(Error::Malformed("truncated fraction"));
    }
    let token = data[end - 1];
    let nlen = data[end - 2] as usize;
    if nlen == 0 || end < nlen + 2 {
        return Err(Error::Malformed("invalid numerator length"));
    }
    let nstart = end - 2 - nlen;
    let n = magnitude_to_decimal(&data[nstart..end - 2]);
    if nstart < 1 {
        return Err(Error::Malformed("missing denominator length"));
    }
    let dlen = data[nstart - 1] as usize;
    if dlen == 0 || nstart < dlen + 1 {
        return Err(Error::Malformed("invalid denominator length"));
    }
    let dstart = nstart - 1 - dlen;
    let denominator = &data[dstart..nstart - 1];
    if denominator.iter().all(|&byte| byte == 0) {
        return Err(Error::Malformed("fraction denominator is zero"));
    }
    let d = magnitude_to_decimal(denominator);
    Ok((
        format!("{}{n}/{d}", if token == 0x22 { "−" } else { "" }),
        dstart,
    ))
}

pub fn encode_float(text: &str) -> Result<Vec<u8>> {
    let normalized = text.replace('ᴇ', "e").replace('−', "-");
    if matches!(
        normalized.to_ascii_lowercase().as_str(),
        "inf" | "+inf" | "-inf" | "nan" | "+nan" | "-nan"
    ) {
        return Err(Error::Malformed("non-finite decimal literal"));
    }
    let (negative, unsigned) = normalized
        .strip_prefix('-')
        .map_or((false, normalized.as_str()), |value| (true, value));
    let unsigned = unsigned.strip_prefix('+').unwrap_or(unsigned);
    let mut pieces = unsigned.split(['e', 'E']);
    let mantissa = pieces.next().unwrap_or_default();
    let explicit_exponent = pieces
        .next()
        .map(|value| {
            value
                .parse::<i64>()
                .map_err(|_| Error::MalformedDetail(format!("invalid float {text}")))
        })
        .transpose()?
        .unwrap_or(0);
    if pieces.next().is_some()
        || mantissa.is_empty()
        || mantissa.bytes().filter(|&byte| byte == b'.').count() > 1
        || !mantissa
            .bytes()
            .all(|byte| byte.is_ascii_digit() || byte == b'.')
    {
        return Err(Error::MalformedDetail(format!("invalid float {text}")));
    }
    let digits = mantissa
        .bytes()
        .filter(u8::is_ascii_digit)
        .collect::<Vec<_>>();
    let Some(first_nonzero) = digits.iter().position(|&digit| digit != b'0') else {
        return Ok(vec![0x40, 0, 0, 0, 0, 0, 0, 0, 0, 0x23]);
    };
    let digits_before_decimal = mantissa.find('.').unwrap_or(mantissa.len());
    let exponent = digits_before_decimal as i64 - first_nonzero as i64 - 1 + explicit_exponent;
    let encoded_exp = 0x4000_i64 + exponent;
    if !(0..=0x7fff).contains(&encoded_exp) {
        return Err(Error::Malformed("floating-point exponent is out of range"));
    }
    let mut significant = digits[first_nonzero..]
        .iter()
        .copied()
        .take(14)
        .collect::<Vec<_>>();
    significant.resize(14, b'0');
    let mut out = Vec::with_capacity(10);
    let hi = ((encoded_exp >> 8) as u8) | if negative { 0x80 } else { 0 };
    out.extend([hi, encoded_exp as u8]);
    for pair in significant.chunks_exact(2) {
        out.push((pair[0] - b'0') << 4 | (pair[1] - b'0'));
    }
    out.push(0x23);
    Ok(out)
}

pub fn decode_float(data: &[u8], end: usize) -> Result<(String, usize)> {
    if end < 10 || data[end - 1] != 0x23 {
        return Err(Error::Malformed("truncated floating-point token"));
    }
    let b = &data[end - 10..end];
    if b[..9] == [0x40, 0, 0, 0, 0, 0, 0, 0, 0] {
        return Ok(("0.".into(), end - 10));
    }
    let negative = b[0] & 0x80 != 0;
    let exponent = ((((b[0] & 0x7f) as i32) << 8) | b[1] as i32) - 0x4000;
    let mut digits = String::with_capacity(14);
    for &packed in &b[2..9] {
        if packed >> 4 > 9 || packed & 15 > 9 {
            return Err(Error::Malformed("invalid BCD floating-point token"));
        }
        digits.push(char::from(b'0' + (packed >> 4)));
        digits.push(char::from(b'0' + (packed & 15)));
    }
    while digits.ends_with('0') {
        digits.pop();
    }
    if digits.is_empty() {
        return Err(Error::Malformed("floating-point token has a zero mantissa"));
    }
    let sign = if negative { "−" } else { "" };
    let rendered = if (-1..14).contains(&exponent) {
        if exponent == -1 {
            format!("{sign}.{}", digits)
        } else {
            let split = (exponent + 1) as usize;
            if split >= digits.len() {
                format!("{sign}{digits}{}.", "0".repeat(split - digits.len()))
            } else {
                format!("{sign}{}.{}", &digits[..split], &digits[split..])
            }
        }
    } else {
        format!("{sign}{}.{}ᴇ{exponent}", &digits[..1], &digits[1..])
    };
    Ok((rendered, end - 10))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn integer_vectors() {
        assert_eq!(encode_integer("0").unwrap(), [0, 0x1f]);
        assert_eq!(encode_integer("256").unwrap(), [0, 1, 2, 0x1f]);
        assert_eq!(encode_integer("1000").unwrap(), [0xe8, 0x03, 2, 0x1f]);
        let b = encode_integer("-42").unwrap();
        assert_eq!(decode_integer(&b, b.len()).unwrap().0, "−42");
        assert_eq!(decode_integer(&[0, 0x1f], 2).unwrap(), ("0".into(), 0));
        assert_eq!(decode_integer(&[0, 0x20], 2).unwrap(), ("−0".into(), 0));
    }
    #[test]
    fn floating_vectors() {
        let b = encode_float("12.5").unwrap();
        assert_eq!(b, [0x40, 1, 0x12, 0x50, 0, 0, 0, 0, 0, 0x23]);
        assert_eq!(decode_float(&b, b.len()).unwrap().0, "12.5");
        assert!(decode_float(&[0x40, 1, 0, 0, 0, 0, 0, 0, 0, 0x23], 10).is_err());

        let negative_scientific = encode_float("−2.5ᴇ250").unwrap();
        let canonical = decode_float(&negative_scientific, negative_scientific.len())
            .unwrap()
            .0;
        assert_eq!(encode_float(&canonical).unwrap(), negative_scientific);
    }
    #[test]
    fn floating_encoding_truncates_decimal_digits_like_vb() {
        assert_eq!(
            encode_float("1.23456789012349").unwrap(),
            [0x40, 0, 0x12, 0x34, 0x56, 0x78, 0x90, 0x12, 0x34, 0x23]
        );
        assert_eq!(
            encode_float("0.00000123456789012349e2").unwrap(),
            [0x3f, 0xfc, 0x12, 0x34, 0x56, 0x78, 0x90, 0x12, 0x34, 0x23]
        );
    }
    #[test]
    fn fraction_vectors() {
        let b = encode_fraction(3, 4, false).unwrap();
        assert_eq!(b, [4, 1, 3, 1, 0x21]);
        assert_eq!(decode_fraction(&b, b.len()).unwrap().0, "3/4");
    }

    #[test]
    fn arbitrary_precision_integer_and_fraction_vectors_are_decoded() {
        let mut huge = vec![0xff; 17];
        huge.push(17);
        huge.push(0x1f);
        let (decimal, start) = decode_integer(&huge, huge.len()).unwrap();
        assert_eq!(start, 0);
        assert_eq!(encode_integer(&decimal).unwrap(), huge);

        let very_large_decimal = "123456789012345678901234567890123456789012345678901234567890";
        let encoded = encode_integer(very_large_decimal).unwrap();
        assert_eq!(
            decode_integer(&encoded, encoded.len()).unwrap(),
            (very_large_decimal.into(), 0)
        );

        let huge_fraction =
            encode_fraction_decimal(very_large_decimal, very_large_decimal, false).unwrap();
        let (fraction, start) = decode_fraction(&huge_fraction, huge_fraction.len()).unwrap();
        assert_eq!(start, 0);
        let (numerator, denominator) = fraction.split_once('/').unwrap();
        assert_eq!(numerator, denominator);
        assert!(numerator.len() > 39);

        assert!(matches!(
            decode_fraction(&[1, 1, 0, 0x21], 4),
            Err(Error::Malformed(msg)) if msg.contains("invalid numerator length")
        ));
        assert!(matches!(
            decode_fraction(&[0, 1, 1, 1, 0x21], 5),
            Err(Error::Malformed(msg)) if msg.contains("denominator is zero")
        ));
    }

    #[test]
    fn floating_point_rejects_non_finite_and_invalid_bcd() {
        assert!(matches!(
            encode_float("inf"),
            Err(Error::Malformed(msg)) if msg.contains("non-finite")
        ));
        assert!(matches!(
            encode_float("NaN"),
            Err(Error::Malformed(msg)) if msg.contains("non-finite")
        ));

        let invalid_bcd = [0x40, 0x01, 0x1a, 0, 0, 0, 0, 0, 0, 0x23];
        assert!(matches!(
            decode_float(&invalid_bcd, invalid_bcd.len()),
            Err(Error::Malformed(msg)) if msg.contains("invalid BCD")
        ));
    }
}
