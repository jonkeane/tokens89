use crate::{number, token, Error, Result, VariableType};
use std::collections::HashSet;

#[derive(Debug, Clone)]
enum Expr {
    Atom(Vec<u8>),
    Unary(u8, Box<Expr>),
    Binary(u8, Box<Expr>, Box<Expr>),
    Call(String, Vec<Expr>, bool),
    Builtin(u8, Vec<Expr>),
    List(Vec<Expr>),
    Postfix(u8, Box<Expr>),
    Subscript(Box<Expr>, Vec<Expr>),
    Matrix(Vec<Vec<Expr>>, Option<u8>),
    Dms(Box<Expr>, Box<Expr>, Box<Expr>),
    BuiltinExtended(u8, Vec<Expr>),
    PrimeCall(String, Vec<Expr>, bool),
    ExtendedBinary(u8, Box<Expr>, Box<Expr>),
    Conversion(Box<Expr>, u8),
}
impl Expr {
    fn emit(&self, out: &mut Vec<u8>) -> Result<()> {
        match self {
            Self::Atom(b) => out.extend(b),
            Self::Unary(t, a) => {
                a.emit(out)?;
                out.push(*t)
            }
            Self::Binary(t, l, r) => {
                if token::binary_operands_are_in_source_order(*t) {
                    l.emit(out)?;
                    r.emit(out)?;
                } else {
                    r.emit(out)?;
                    l.emit(out)?;
                }
                out.push(*t)
            }
            Self::Call(name, args, local) => {
                out.push(0xe5);
                for a in args.iter().rev() {
                    a.emit(out)?;
                }
                encode_variable(name, out);
                if *local {
                    out.push(0xf0);
                }
                out.push(0xda);
            }
            Self::Builtin(token, args) => {
                if token::builtin_arity(*token).is_none() {
                    out.push(0xe5);
                }
                for arg in args.iter().rev() {
                    arg.emit(out)?;
                }
                out.push(*token);
            }
            Self::List(items) => {
                out.push(0xe5);
                for item in items.iter().rev() {
                    item.emit(out)?;
                }
                out.push(0xd9);
            }
            Self::Postfix(token, value) => {
                value.emit(out)?;
                out.push(*token);
            }
            Self::Subscript(value, indices) => {
                out.push(0xe5);
                for index in indices.iter().rev() {
                    index.emit(out)?;
                }
                value.emit(out)?;
                out.push(0xd5);
            }
            Self::Matrix(rows, vector) => {
                out.push(0xe5);
                for row in rows.iter().rev() {
                    out.push(0xe5);
                    for item in row.iter().rev() {
                        item.emit(out)?;
                    }
                    out.push(0xd9);
                }
                out.push(0xd9);
                if let Some(token) = vector {
                    out.push(*token);
                }
            }
            Self::Dms(degrees, minutes, seconds) => {
                seconds.emit(out)?;
                minutes.emit(out)?;
                degrees.emit(out)?;
                out.push(0xcd);
            }
            Self::BuiltinExtended(token, args) => {
                if token::extended_arity(*token).is_none() {
                    out.push(0xe5);
                }
                for arg in args.iter().rev() {
                    arg.emit(out)?;
                }
                out.extend([*token, 0xe3]);
            }
            Self::PrimeCall(name, args, local) => {
                out.push(0xe5);
                for arg in args.iter().rev() {
                    arg.emit(out)?;
                }
                encode_variable(name, out);
                if *local {
                    out.push(0xf0);
                }
                out.extend([0xda, 0xf2]);
            }
            Self::ExtendedBinary(sub, left, right) => {
                // The legacy VB queues the right operand first for all three
                // extended binary forms. Because tokens are prepended to its
                // output, their final streams contain left then right.
                left.emit(out)?;
                right.emit(out)?;
                out.extend([*sub, 0xe3]);
            }
            Self::Conversion(value, sub) => {
                value.emit(out)?;
                out.extend([*sub, 0xe3]);
            }
        }
        Ok(())
    }
}

struct Parser<'a> {
    s: &'a [u8],
    p: usize,
    locals: Option<&'a HashSet<String>>,
}
impl<'a> Parser<'a> {
    fn new(s: &'a [u8]) -> Self {
        Self {
            s,
            p: 0,
            locals: None,
        }
    }
    fn with_locals(s: &'a [u8], locals: &'a HashSet<String>) -> Self {
        Self {
            s,
            p: 0,
            locals: Some(locals),
        }
    }
    fn is_local(&self, name: &str) -> bool {
        self.locals
            .is_some_and(|set| set.contains(&name.to_lowercase()))
    }
    fn ws(&mut self) {
        while self.p < self.s.len() && self.s[self.p].is_ascii_whitespace() {
            self.p += 1
        }
    }
    fn expr(&mut self, min: u8) -> Result<Expr> {
        self.ws();
        let mut left = self.primary()?;
        loop {
            self.ws();
            let (start, explicit_op) = self.peek_op();
            let implicit = explicit_op.is_none() && self.starts_primary(start);
            let Some(op) = explicit_op.or(implicit.then_some("*")) else {
                break;
            };
            let prec = token::precedence(op);
            if prec < min {
                break;
            }
            self.p = if implicit {
                start
            } else {
                start
                    + if matches!(op, "≤" | "≥" | "≠" | "→" | "±" | "∠") {
                        1
                    } else {
                        op.len()
                    }
            };
            // VB scans most precedence groups from right to left, selecting
            // the rightmost operator and therefore making them left
            // associative. Concatenation and powers are scanned left to right.
            let right = self.expr(if matches!(op, "&" | "^" | ".^") {
                prec
            } else {
                prec + 1
            })?;
            left = if op == "^" && matches!(&left, Expr::Atom(bytes) if bytes == &[0x25]) {
                Expr::Unary(0x52, Box::new(right))
            } else if op == "&" {
                Expr::ExtendedBinary(0x14, Box::new(left), Box::new(right))
            } else if op == "∠" {
                Expr::ExtendedBinary(0x26, Box::new(left), Box::new(right))
            } else if op == "/" {
                Expr::Binary(0x91, Box::new(left), Box::new(right))
            } else {
                Expr::Binary(
                    token::binary_token(op).unwrap(),
                    Box::new(left),
                    Box::new(right),
                )
            };
        }
        Ok(left)
    }
    fn starts_primary(&self, mut p: usize) -> bool {
        while p < self.s.len() && self.s[p].is_ascii_whitespace() {
            p += 1;
        }
        self.s.get(p).is_some_and(|b| {
            b.is_ascii_alphanumeric()
                || matches!(
                    *b,
                    b'_' | b'('
                        | b'{'
                        | b'['
                        | b'"'
                        | b'@'
                        | b'#'
                        | 140
                        | 150
                        | 151
                        | 159
                        | 177
                        | 190
                )
                || (*b >= 128 && !matches!(*b, 152 | 153 | 156 | 157 | 158))
        })
    }
    fn peek_op(&self) -> (usize, Option<&'static str>) {
        let mut p = self.p;
        while p < self.s.len() && self.s[p].is_ascii_whitespace() {
            p += 1
        }
        if p < self.s.len() {
            let op = match self.s[p] {
                156 => Some("≤"),
                157 => Some("≠"),
                158 => Some("≥"),
                22 => Some("→"),
                177 => Some("±"),
                159 => Some("∠"),
                _ => None,
            };
            if op.is_some() {
                return (p, op);
            }
        }
        const OPS: [&str; 25] = [
            "./", ".*", ".+", ".-", ".^", "<=", ">=", "!=", "/=", "->", "xor", "and", "or", "+",
            "-", "*", "/", "^", "=", "<", ">", "≤", "≥", "|", "&",
        ];
        for op in OPS {
            if self.s[p..].starts_with(op.as_bytes()) {
                return (p, Some(op));
            }
        }
        (p, None)
    }
    fn primary(&mut self) -> Result<Expr> {
        self.ws();
        if self.p >= self.s.len() {
            return Err(Error::Malformed("expected expression"));
        }
        if self.s[self.p] == b'(' {
            self.p += 1;
            let e = self.expr(1)?;
            self.ws();
            if self.s.get(self.p) != Some(&b')') {
                return Err(Error::Malformed("unclosed parenthesis"));
            }
            self.p += 1;
            return self.postfix(e);
        }
        if self.s[self.p] == b'@' {
            self.p += 1;
            let integer = self
                .s
                .get(self.p)
                .is_some_and(|b| b.eq_ignore_ascii_case(&b'n'));
            if integer {
                self.p += 1;
            }
            let start = self.p;
            while self.s.get(self.p).is_some_and(u8::is_ascii_digit) {
                self.p += 1;
            }
            if start == self.p {
                return Err(Error::Malformed("arbitrary constant requires an index"));
            }
            let index = std::str::from_utf8(&self.s[start..self.p])
                .unwrap()
                .parse::<u8>()
                .map_err(|_| Error::Malformed("arbitrary constant index exceeds 255"))?;
            return self.postfix(Expr::Atom(vec![index, if integer { 0x1e } else { 0x1d }]));
        }
        if self.s[self.p] == b'#' {
            self.p += 1;
            let value = self.primary()?;
            return self.postfix(Expr::BuiltinExtended(0x01, vec![value]));
        }
        if self.s[self.p] == b'{' {
            self.p += 1;
            let mut items = Vec::new();
            self.ws();
            if self.s.get(self.p) != Some(&b'}') {
                loop {
                    items.push(self.expr(1)?);
                    self.ws();
                    if self.s.get(self.p) == Some(&b',') {
                        self.p += 1;
                        continue;
                    }
                    break;
                }
            }
            if self.s.get(self.p) != Some(&b'}') {
                return Err(Error::Malformed("unclosed list"));
            }
            self.p += 1;
            return self.postfix(Expr::List(items));
        }
        if self.s[self.p] == b'[' && !self.s[self.p..].starts_with(b"[[") {
            self.p += 1;
            let mut rows = vec![Vec::new()];
            if self.s.get(self.p) != Some(&b']') {
                loop {
                    rows.last_mut().unwrap().push(self.expr(1)?);
                    self.ws();
                    match self.s.get(self.p) {
                        Some(b',') => self.p += 1,
                        Some(b';') => {
                            self.p += 1;
                            rows.push(Vec::new());
                        }
                        _ => break,
                    }
                }
            }
            if self.s.get(self.p) != Some(&b']') {
                return Err(Error::Malformed("unclosed vector or matrix"));
            }
            self.p += 1;
            let mut angle_positions = Vec::new();
            for (i, item) in rows[0].iter().enumerate() {
                if let Expr::Unary(0x7e, _) = item {
                    angle_positions.push(i);
                }
            }
            let vector = match angle_positions.as_slice() {
                [1] if rows[0].len() == 2 => Some(0x7b),
                [1] if rows[0].len() == 3 => Some(0x7c),
                [1, 2] if rows[0].len() == 3 => Some(0x7d),
                _ => None,
            };
            for row in &mut rows {
                for item in row {
                    if let Expr::Unary(0x7e, value) = item {
                        *item = *value.clone();
                    }
                }
            }
            return self.postfix(Expr::Matrix(rows, vector));
        }
        if self.s[self.p..].starts_with(b"[[") {
            self.p += 1;
            let mut rows = Vec::new();
            while self.s.get(self.p) == Some(&b'[') {
                self.p += 1;
                let mut row = Vec::new();
                if self.s.get(self.p) != Some(&b']') {
                    loop {
                        row.push(self.expr(1)?);
                        self.ws();
                        if self.s.get(self.p) == Some(&b',') {
                            self.p += 1;
                            continue;
                        }
                        break;
                    }
                }
                if self.s.get(self.p) != Some(&b']') {
                    return Err(Error::Malformed("unclosed matrix row"));
                }
                self.p += 1;
                rows.push(row);
                self.ws();
                if self.s.get(self.p) == Some(&b',') {
                    self.p += 1;
                    self.ws();
                }
            }
            if self.s.get(self.p) != Some(&b']') {
                return Err(Error::Malformed("unclosed matrix"));
            }
            self.p += 1;
            return self.postfix(Expr::Matrix(rows, None));
        }
        if self.s[self.p] == b'"' {
            self.p += 1;
            let mut b = vec![0];
            loop {
                if self.p >= self.s.len() {
                    return Err(Error::Malformed("unclosed string"));
                }
                if self.s[self.p] == b'"' {
                    if self.s.get(self.p + 1) == Some(&b'"') {
                        b.push(b'"');
                        self.p += 2;
                        continue;
                    }
                    break;
                }
                b.push(self.s[self.p]);
                self.p += 1;
            }
            b.extend([0, 0x2d]);
            self.p += 1;
            return self.postfix(Expr::Atom(b));
        }
        if self.s[self.p].is_ascii_digit()
            || (self.s[self.p] == b'.' && self.s.get(self.p + 1).is_some_and(u8::is_ascii_digit))
        {
            let start = self.p;
            let based = self.s.get(self.p..self.p + 2).is_some_and(|prefix| {
                prefix.eq_ignore_ascii_case(b"0b") || prefix.eq_ignore_ascii_case(b"0h")
            });
            if based {
                self.p += 2;
                while self.s.get(self.p).is_some_and(u8::is_ascii_alphanumeric) {
                    self.p += 1;
                }
            } else {
                while self
                    .s
                    .get(self.p)
                    .is_some_and(|b| b.is_ascii_digit() || *b == b'.')
                {
                    self.p += 1;
                }
                if self
                    .s
                    .get(self.p)
                    .is_some_and(|b| matches!(*b, b'e' | b'E' | 149))
                {
                    self.p += 1;
                    if self
                        .s
                        .get(self.p)
                        .is_some_and(|b| matches!(*b, b'+' | b'-' | 173))
                    {
                        self.p += 1;
                    }
                    while self.s.get(self.p).is_some_and(u8::is_ascii_digit) {
                        self.p += 1;
                    }
                }
            }
            let text = self.s[start..self.p]
                .iter()
                .map(|&b| match b {
                    149 => 'e',
                    173 => '-',
                    _ => b as char,
                })
                .collect::<String>();
            let mut atom = if !based && (text.contains('.') || text.contains('e')) {
                number::encode_float(&text)?
            } else {
                number::encode_integer(&text)?
            };
            if text.len() > 2 && text[..2].eq_ignore_ascii_case("0b") {
                atom.extend([0x2b, 0xe3]);
            } else if text.len() > 2 && text[..2].eq_ignore_ascii_case("0h") {
                atom.extend([0x2c, 0xe3]);
            }
            let value = Expr::Atom(atom);
            if self.s.get(self.p) == Some(&0xb0) {
                self.p += 1;
                let minutes = self.primary()?;
                let minutes = if let Expr::Postfix(0xef, value) = minutes {
                    *value
                } else {
                    if self.s.get(self.p) != Some(&b'\'') {
                        return Err(Error::Malformed("DMS literal is missing minute mark"));
                    }
                    self.p += 1;
                    minutes
                };
                let seconds = self.primary()?;
                if self.s.get(self.p) != Some(&b'"') {
                    return Err(Error::Malformed("DMS literal is missing second mark"));
                }
                self.p += 1;
                return self.postfix(Expr::Dms(
                    Box::new(value),
                    Box::new(minutes),
                    Box::new(seconds),
                ));
            }
            return self.postfix(value);
        }
        if self.s[self.p] == b'-' || self.s[self.p] == 173 {
            self.p += 1;
            if self.s.get(self.p) == Some(&190) {
                self.p += 1;
                return self.postfix(Expr::Atom(vec![0x27]));
            }
            let a = self.primary()?;
            return self.postfix(Expr::Unary(0x7a, Box::new(a)));
        }
        if self.s[self.p] == 177 {
            self.p += 1;
            let value = self.primary()?;
            return self.postfix(Expr::Unary(0xea, Box::new(value)));
        }
        if self.s[self.p] == 159 {
            self.p += 1;
            let value = self.expr(1)?;
            return Ok(Expr::Unary(0x7e, Box::new(value)));
        }
        if matches!(self.s[self.p], 140 | 150 | 151 | 190) {
            let atom = match self.s[self.p] {
                140 => vec![0x24],
                150 => vec![0x25],
                151 => vec![0x26],
                _ => vec![0x28],
            };
            self.p += 1;
            return self.postfix(Expr::Atom(atom));
        }
        let start = self.p;
        while self.p < self.s.len()
            && (self.s[self.p].is_ascii_alphanumeric()
                || self.s[self.p] == b'_'
                // A user-defined variable or function in another folder is
                // written as `folder\\name`. Keep the pathname together so it
                // is emitted as one calculator variable token.
                || self.s[self.p] == b'\\'
                // TI-Graph Link exports the same folder separator as a dot.
                || (self.s[self.p] == b'.'
                    && self.s.get(self.p + 1).is_some_and(u8::is_ascii_alphanumeric))
                || (self.s[self.p] >= 128
                    && !matches!(self.s[self.p], 152 | 153 | 156 | 157 | 158 | 159 | 177))
                || (self.s[self.p] == 18 && self.s[self.p..].contains(&b'(')))
        {
            self.p += 1
        }
        if start == self.p {
            return Err(Error::MalformedDetail(format!(
                "unexpected source byte 0x{:02x}",
                self.s[self.p]
            )));
        }
        let name_bytes = &self.s[start..self.p];
        if name_bytes.iter().any(|&b| b >= 128) && self.s.get(self.p) != Some(&b'(') {
            let mut atom = match name_bytes {
                [140] => vec![0x24],
                [150] => vec![0x25],
                [151] => vec![0x26],
                [190] => vec![0x28],
                _ => {
                    let mut b = Vec::new();
                    encode_variable_bytes(name_bytes, &mut b);
                    b
                }
            };
            if self.is_local(&crate::charset::decode_source(name_bytes)) {
                atom.push(0xf0);
            }
            return self.postfix(Expr::Atom(atom));
        }
        let name = crate::charset::decode_source(name_bytes);
        let prime = self.s.get(self.p) == Some(&b'\'') && self.s.get(self.p + 1) == Some(&b'(');
        if prime {
            self.p += 1;
        }
        self.ws();
        if self.s.get(self.p) == Some(&b'(') {
            self.p += 1;
            let mut args = Vec::new();
            let mut semicolon = false;
            self.ws();
            if self.s.get(self.p) != Some(&b')') {
                loop {
                    if matches!(self.s.get(self.p), Some(b',') | Some(b')')) {
                        args.push(Expr::Atom(vec![0x2e]));
                    } else {
                        args.push(self.expr(1)?);
                    }
                    self.ws();
                    if matches!(self.s.get(self.p), Some(b',') | Some(b';')) {
                        semicolon |= self.s[self.p] == b';';
                        self.p += 1;
                        continue;
                    }
                    break;
                }
            }
            if self.s.get(self.p) != Some(&b')') {
                return Err(Error::Malformed("unclosed function call"));
            }
            self.p += 1;
            if prime {
                if semicolon {
                    return Err(Error::Malformed("prime function arguments require commas"));
                }
                let local = self.is_local(&name);
                return self.postfix(Expr::PrimeCall(name, args, local));
            }
            let lower = name.to_lowercase();
            if semicolon {
                if lower == "augment" && args.len() == 2 {
                    return self.postfix(Expr::BuiltinExtended(0x35, args));
                }
                return Err(Error::Malformed(
                    "semicolon arguments are only valid for augment",
                ));
            }
            if let Some(t) = token::unary_token(&name) {
                if args.len() == 1 {
                    return self.postfix(Expr::Unary(t, Box::new(args.remove(0))));
                }
            }
            // Several AMS functions share a source spelling and select their
            // ordinary or extended token based on arity.
            if lower == "simult" && args.len() == 3 {
                return self.postfix(Expr::BuiltinExtended(0x33, args));
            }
            if lower == "simult" && args.len() > 3 {
                return Err(Error::Malformed("simult accepts two or three arguments"));
            }
            if let Some(t) = token::builtin_token(&name) {
                return self.postfix(Expr::Builtin(t, args));
            }
            if let Some(t) = token::extended_token(&name) {
                return self.postfix(Expr::BuiltinExtended(t, args));
            }
            let local = self.is_local(&name);
            return self.postfix(Expr::Call(name, args, local));
        }
        let atom = match name.to_ascii_lowercase().as_str() {
            "true" => vec![0x2c],
            "false" => vec![0x2b],
            "undef" => vec![0x2a],
            _ => {
                if let Some(code) = token::system_variable_token(&name) {
                    vec![code, 0x1c]
                } else {
                    let mut b = Vec::new();
                    encode_variable(&name, &mut b);
                    if self.is_local(&name) {
                        b.push(0xf0);
                    }
                    b
                }
            }
        };
        self.postfix(Expr::Atom(atom))
    }
    fn postfix(&mut self, mut value: Expr) -> Result<Expr> {
        loop {
            self.ws();
            match self.s.get(self.p).copied() {
                Some(b'!') => {
                    self.p += 1;
                    value = Expr::Postfix(0x76, Box::new(value));
                }
                Some(b'%') => {
                    self.p += 1;
                    value = Expr::Postfix(0x77, Box::new(value));
                }
                Some(b'\'') => {
                    self.p += 1;
                    value = Expr::Postfix(0xef, Box::new(value));
                }
                Some(18) => {
                    self.p += 1;
                    if self.s.get(self.p) == Some(&b'_') {
                        let start = self.p;
                        self.p += 1;
                        while self
                            .s
                            .get(self.p)
                            .is_some_and(|b| b.is_ascii_alphanumeric() || *b == b'_')
                        {
                            self.p += 1;
                        }
                        let mut raw = Vec::new();
                        encode_variable_bytes(&self.s[start..self.p], &mut raw);
                        value =
                            Expr::ExtendedBinary(0x05, Box::new(value), Box::new(Expr::Atom(raw)));
                        continue;
                    }
                    let start = self.p;
                    while self.s.get(self.p).is_some_and(u8::is_ascii_alphabetic) {
                        self.p += 1;
                    }
                    let name = std::str::from_utf8(&self.s[start..self.p]).unwrap();
                    if let Some(sub) = conversion_token(name) {
                        value = Expr::Conversion(Box::new(value), sub);
                    } else {
                        self.p = start;
                        let target = self.primary()?;
                        value = Expr::ExtendedBinary(0x05, Box::new(value), Box::new(target));
                    }
                }
                Some(152) => {
                    self.p += 1;
                    value = Expr::Postfix(0x78, Box::new(value));
                }
                Some(153) => {
                    self.p += 1;
                    value = Expr::Postfix(0x75, Box::new(value));
                }
                Some(b'[') => {
                    self.p += 1;
                    let mut indices = Vec::new();
                    loop {
                        indices.push(self.expr(1)?);
                        self.ws();
                        if self.s.get(self.p) == Some(&b',') {
                            self.p += 1;
                            continue;
                        }
                        break;
                    }
                    if self.s.get(self.p) != Some(&b']') {
                        return Err(Error::Malformed("unclosed subscript"));
                    }
                    self.p += 1;
                    value = Expr::Subscript(Box::new(value), indices);
                }
                _ => break,
            }
        }
        Ok(value)
    }
}
fn conversion_token(name: &str) -> Option<u8> {
    Some(match name.to_ascii_lowercase().as_str() {
        "dd" => 0x15,
        "dms" => 0x16,
        "rect" => 0x17,
        "polar" => 0x18,
        "cylind" => 0x19,
        "sphere" => 0x1a,
        "bin" => 0x2d,
        "dec" => 0x2e,
        "hex" => 0x2f,
        "grad" => 0x5f,
        "rad" => 0x60,
        _ => return None,
    })
}

fn encode_variable(name: &str, out: &mut Vec<u8>) {
    encode_variable_bytes(name.as_bytes(), out)
}
fn encode_variable_bytes(name: &[u8], out: &mut Vec<u8>) {
    if name.len() == 1 {
        let c = name[0].to_ascii_lowercase();
        let t = if (b'a'..=b'q').contains(&c) {
            c - b'a' + 0x0b
        } else if (b'r'..=b'z').contains(&c) {
            c - b'r' + 2
        } else {
            0
        };
        if t != 0 {
            out.push(t);
            return;
        }
    }
    out.push(0);
    out.extend(name.iter().map(|b| crate::charset::lowercase_byte(*b)));
    out.push(0)
}
fn expression_body(source: &[u8]) -> Result<Vec<u8>> {
    let mut p = Parser::new(source);
    let e = p.expr(1)?;
    p.ws();
    if p.p != source.len() {
        return Err(Error::MalformedDetail(format!(
            "unexpected input at byte {}",
            p.p
        )));
    }
    let mut out = Vec::new();
    e.emit(&mut out)?;
    Ok(out)
}
fn expression_body_with_locals(source: &[u8], locals: &HashSet<String>) -> Result<Vec<u8>> {
    let mut p = Parser::with_locals(source, locals);
    let e = p.expr(1)?;
    p.ws();
    if p.p != source.len() {
        return Err(Error::MalformedDetail(format!(
            "unexpected input at byte {}",
            p.p
        )));
    }
    let mut out = Vec::new();
    e.emit(&mut out)?;
    Ok(out)
}

fn expression(source: &[u8]) -> Result<Vec<u8>> {
    let mut out = vec![token::END_STACK];
    out.extend(expression_body(source)?);
    Ok(out)
}

fn split_arguments(input: &[u8]) -> Result<Vec<&[u8]>> {
    if input.iter().all(u8::is_ascii_whitespace) {
        return Ok(Vec::new());
    }
    let mut result = Vec::new();
    let mut start = 0;
    let mut depth = 0i32;
    let mut string = false;
    for (i, &b) in input.iter().enumerate() {
        match b {
            b'"' => string = !string,
            b'(' | b'{' | b'[' if !string => depth += 1,
            b')' | b'}' | b']' if !string => depth -= 1,
            b',' | b';' if !string && depth == 0 => {
                result.push(&input[start..i]);
                start = i + 1;
            }
            _ => {}
        }
    }
    if string || depth != 0 {
        return Err(Error::Malformed("unbalanced instruction arguments"));
    }
    result.push(&input[start..]);
    Ok(result)
}

fn instruction(line: &[u8], locals: Option<&HashSet<String>>) -> Result<Option<(Vec<u8>, u8)>> {
    let trimmed = line.strip_prefix(b" ").unwrap_or(line);
    let split = trimmed
        .iter()
        .position(|b| b.is_ascii_whitespace())
        .unwrap_or(trimmed.len());
    let Ok(word) = std::str::from_utf8(&trimmed[..split]) else {
        return Ok(None);
    };
    let mut args = trimmed[split..]
        .iter()
        .copied()
        .skip_while(u8::is_ascii_whitespace)
        .collect::<Vec<_>>();
    let has_then = args.len() >= 4 && args[args.len() - 4..].eq_ignore_ascii_case(b"then");
    if has_then {
        args.truncate(args.len() - 4);
        while args.last().is_some_and(u8::is_ascii_whitespace) {
            args.pop();
        }
    }
    let Some(mut code) = token::instruction_token(word, has_then) else {
        return Ok(None);
    };
    if code == 0x86 {
        return define_instruction(word, &args, locals).map(Some);
    }
    let mut values = split_arguments(&args)?;
    if code == 0x43 && values.len() == 3 {
        code = 0x99;
    }
    let expected = token::instruction_arity(code);
    if let Some(n) = expected {
        if values.len() > n {
            return Err(Error::MalformedDetail(format!(
                "{word} expects at most {n} argument(s), found {}",
                values.len()
            )));
        }
        values.resize(n, b"");
    }
    let mut out = Vec::new();
    if expected.is_none() {
        out.push(0xe5);
    }
    for arg in values.iter().rev() {
        out.extend(if arg.iter().all(u8::is_ascii_whitespace) {
            vec![0x2e]
        } else if let Some(locals) = locals {
            expression_body_with_locals(arg, locals)?
        } else {
            expression_body(arg)?
        });
    }
    if token::has_displacement(code) {
        out.extend([0, 0]);
    }
    out.extend([code, 0xe4]);
    Ok(Some((out, code)))
}

fn define_instruction(
    word: &str,
    args: &[u8],
    outer_locals: Option<&HashSet<String>>,
) -> Result<(Vec<u8>, u8)> {
    let mut depth = 0i32;
    let mut string = false;
    let equals = args.iter().enumerate().find_map(|(index, &byte)| {
        match byte {
            b'"' => string = !string,
            b'(' | b'{' | b'[' if !string => depth += 1,
            b')' | b'}' | b']' if !string => depth -= 1,
            b'=' if !string && depth == 0 => return Some(index),
            _ => {}
        }
        None
    });
    let equals =
        equals.ok_or_else(|| Error::MalformedDetail(format!("{word} requires a top-level '='")))?;
    let target = &args[..equals];
    let body = &args[equals + 1..];
    if target.iter().all(u8::is_ascii_whitespace) || body.iter().all(u8::is_ascii_whitespace) {
        return Err(Error::Malformed("Define requires both a target and a body"));
    }

    let mut body_locals = outer_locals.cloned().unwrap_or_default();
    let trimmed = target
        .iter()
        .copied()
        .skip_while(u8::is_ascii_whitespace)
        .collect::<Vec<_>>();
    if let Some(open) = trimmed.iter().position(|&b| b == b'(') {
        if trimmed.last() == Some(&b')') {
            for parameter in split_arguments(&trimmed[open + 1..trimmed.len() - 1])? {
                let name = crate::charset::decode_source(parameter)
                    .trim()
                    .to_lowercase();
                if !name.is_empty() {
                    body_locals.insert(name);
                }
            }
        }
    }

    let mut out = expression_body_with_locals(body, &body_locals)?;
    out.extend(if let Some(locals) = outer_locals {
        expression_body_with_locals(target, locals)?
    } else {
        expression_body(target)?
    });
    out.extend([0x86, 0xe4]);
    Ok((out, 0x86))
}

fn function_is_multiline(source: &[u8]) -> bool {
    let source = source.strip_suffix(b"\r").unwrap_or(source);
    source
        .split(|&b| b == b'\r')
        .nth(1)
        .is_some_and(|line| line.eq_ignore_ascii_case(b"Func"))
}

fn tokenized_one_line_function(source: &[u8]) -> Result<Vec<u8>> {
    let source = source.strip_suffix(b"\r").unwrap_or(source);
    let lines = source.split(|&b| b == b'\r').collect::<Vec<_>>();
    if lines.len() != 2 {
        return Err(Error::Malformed(
            "one-line function requires an argument line and one expression",
        ));
    }
    let args = lines[0]
        .strip_prefix(b"(")
        .and_then(|value| value.strip_suffix(b")"))
        .ok_or(Error::Malformed(
            "program/function first line must be an argument list",
        ))?;
    let arg_values = split_arguments(args)?;
    let locals = arg_values
        .iter()
        .map(|value| crate::charset::decode_source(value).trim().to_lowercase())
        .filter(|value| !value.is_empty())
        .collect::<HashSet<_>>();

    let mut out = vec![token::END_STACK];
    out.extend(expression_body_with_locals(lines[1], &locals)?);
    out.push(0xe5);
    for arg in arg_values.iter().rev() {
        out.extend(expression_body(arg)?);
    }
    out.extend([0, 0, 0, 0xdc]);
    Ok(out)
}

fn tokenized_program(source: &[u8], kind: VariableType) -> Result<Vec<u8>> {
    let source = source.strip_suffix(b"\r").unwrap_or(source);
    let lines = source.split(|&b| b == b'\r').collect::<Vec<_>>();
    if lines.len() < 2 {
        return Err(Error::Malformed(
            "program source requires an argument line and body",
        ));
    }
    let args = lines[0]
        .strip_prefix(b"(")
        .and_then(|v| v.strip_suffix(b")"))
        .ok_or(Error::Malformed(
            "program/function first line must be an argument list",
        ))?;
    let arg_values = split_arguments(args)?;
    let mut locals = arg_values
        .iter()
        .map(|v| crate::charset::decode_source(v).trim().to_lowercase())
        .filter(|v| !v.is_empty())
        .collect::<HashSet<_>>();
    let mut wrapper = vec![0xe5];
    for arg in arg_values.iter().rev() {
        wrapper.extend(expression_body(arg)?);
    }
    wrapper.extend([0, 0, 0, 0xdc]);
    let mut logical = Vec::new();
    for (line_index, line) in lines[1..].iter().enumerate() {
        for (index, part) in split_colons(line).into_iter().enumerate() {
            logical.push((part, if index == 0 { 0xe8 } else { 0xe7 }, line_index + 2));
        }
    }
    let mut encoded = Vec::new();
    for (line, delimiter, line_number) in logical {
        let indent = line.iter().take_while(|&&b| b == b' ').count().min(255) as u8;
        let body = &line[indent as usize..];
        let comment = comment_position(body);
        let (statement_raw, comment_text) =
            comment.map_or((body, None), |at| (&body[..at], Some(&body[at + 1..])));
        let trailing = statement_raw
            .iter()
            .rev()
            .take_while(|&&b| b == b' ')
            .count()
            .min(255) as u8;
        let statement = &statement_raw[..statement_raw.len() - trailing as usize];
        let (parsed_segment, instruction_code) = if let Some(raw) = raw_token_escape(statement)? {
            (raw, None)
        } else if statement.iter().all(u8::is_ascii_whitespace) {
            (Vec::new(), None)
        } else if let Some((value, code)) = instruction(statement, Some(&locals))
            .map_err(|error| Error::MalformedDetail(format!("line {line_number}: {error}")))?
        {
            (value, Some(code))
        } else {
            (
                expression_body_with_locals(statement, &locals).map_err(|error| {
                    Error::MalformedDetail(format!("line {line_number}: {error}"))
                })?,
                None,
            )
        };
        let mut segment = parsed_segment;
        let line_indent = if let Some(text) = comment_text {
            let only_comment = statement.iter().all(u8::is_ascii_whitespace);
            let comment_indent = if only_comment { indent } else { trailing };
            let mut with_comment = vec![0];
            with_comment.extend(text);
            with_comment.extend([0, comment_indent, 0xe6]);
            with_comment.extend(segment);
            segment = with_comment;
            if only_comment {
                0
            } else {
                indent
            }
        } else {
            indent
        };
        encoded.push((segment, line_indent, delimiter, instruction_code));
        if statement.len() > 6
            && statement[..5].eq_ignore_ascii_case(b"local")
            && statement[5].is_ascii_whitespace()
        {
            if let Ok(values) = split_arguments(&statement[6..]) {
                for value in values {
                    locals.insert(crate::charset::decode_source(value).trim().to_lowercase());
                }
            }
        }
    }
    if !encoded.iter().any(|(s, _, _, _)| {
        s.ends_with(&[
            if matches!(kind, VariableType::Program) {
                0x19
            } else {
                0x17
            },
            0xe4,
        ])
    }) {
        return Err(Error::Malformed(
            "program/function body is missing Prgm or Func",
        ));
    }
    let mut out = vec![0xe9];
    let mut instructions = Vec::new();
    for (i, (segment, _, _, instruction_code)) in encoded.iter().enumerate().rev() {
        let start = out.len();
        out.extend(segment);
        if let Some(code) = instruction_code {
            instructions.push((start + segment.len() - 2, *code));
        }
        if i > 0 {
            out.extend([encoded[i].1, encoded[i].2]);
        }
    }
    out.extend(wrapper);
    patch_branches(&mut out, &instructions)?;
    Ok(out)
}

fn raw_token_escape(statement: &[u8]) -> Result<Option<Vec<u8>>> {
    let Some(hex) = statement
        .strip_prefix(b"@tokens(")
        .and_then(|value| value.strip_suffix(b")"))
    else {
        return Ok(None);
    };
    if hex.len() % 2 != 0 || !hex.iter().all(u8::is_ascii_hexdigit) {
        return Err(Error::Malformed(
            "@tokens escape requires pairs of hexadecimal digits",
        ));
    }
    let mut bytes = Vec::with_capacity(hex.len() / 2);
    for pair in hex.chunks_exact(2) {
        let text = std::str::from_utf8(pair).unwrap();
        bytes.push(u8::from_str_radix(text, 16).unwrap());
    }
    Ok(Some(bytes))
}

fn comment_position(line: &[u8]) -> Option<usize> {
    let mut string = false;
    for (i, &b) in line.iter().enumerate() {
        if b == b'"' {
            string = !string;
        } else if b == 169 && !string {
            return Some(i);
        }
    }
    None
}

fn split_colons(line: &[u8]) -> Vec<&[u8]> {
    let mut parts = Vec::new();
    let mut start = 0;
    let mut string = false;
    for (i, &b) in line.iter().enumerate() {
        if b == b'"' {
            string = !string;
        } else if b == 169 && !string {
            break;
        } else if b == b':' && !string {
            parts.push(&line[start..i]);
            start = i + 1;
        }
    }
    parts.push(&line[start..]);
    parts
}

fn patch_branches(bytes: &mut [u8], instructions: &[(usize, u8)]) -> Result<()> {
    patch_structured_else(bytes, instructions)?;
    struct Block {
        code: u8,
        open: usize,
        exits: Vec<usize>,
        cycles: Vec<usize>,
    }
    let mut blocks: Vec<Block> = Vec::new();
    for &(at, code) in instructions.iter().rev() {
        match code {
            0x18 | 0x3d | 0x74 => blocks.push(Block {
                code,
                open: at,
                exits: Vec::new(),
                cycles: Vec::new(),
            }),
            0x07 => {
                if let Some(block) = blocks.last_mut() {
                    block.cycles.push(at)
                } else {
                    return Err(Error::Malformed("Cycle outside a loop"));
                }
            }
            0x16 => {
                if let Some(block) = blocks.last_mut() {
                    block.exits.push(at)
                } else {
                    return Err(Error::Malformed("Exit outside a loop"));
                }
            }
            0x0e | 0x11 | 0x15 => {
                let expected = match code {
                    0x0e => 0x74,
                    0x11 => 0x18,
                    _ => 0x3d,
                };
                let block = blocks
                    .pop()
                    .ok_or(Error::Malformed("block terminator without opener"))?;
                if block.code != expected {
                    return Err(Error::Malformed("mismatched loop terminator"));
                }
                let d = u16::try_from(
                    block
                        .open
                        .abs_diff(at)
                        .checked_add(2)
                        .ok_or(Error::Malformed("branch displacement overflow"))?,
                )
                .map_err(|_| Error::Malformed("branch displacement exceeds 65535 bytes"))?;
                if at < 2 {
                    return Err(Error::Malformed("missing branch displacement"));
                }
                bytes[at - 2..at].copy_from_slice(&d.to_le_bytes());
                for jump_at in block.exits {
                    let jump = u16::try_from(
                        jump_at
                            .abs_diff(at)
                            .checked_add(2)
                            .ok_or(Error::Malformed("Exit displacement overflow"))?,
                    )
                    .map_err(|_| Error::Malformed("Exit displacement exceeds 65535 bytes"))?;
                    if jump_at < 2 {
                        return Err(Error::Malformed("missing jump displacement"));
                    }
                    bytes[jump_at - 2..jump_at].copy_from_slice(&jump.to_le_bytes());
                }
                for jump_at in block.cycles {
                    let jump = u16::try_from(
                        jump_at
                            .abs_diff(at)
                            .checked_sub(2)
                            .ok_or(Error::Malformed("invalid Cycle displacement"))?,
                    )
                    .map_err(|_| Error::Malformed("Cycle displacement exceeds 65535 bytes"))?;
                    if jump_at < 2 {
                        return Err(Error::Malformed("missing jump displacement"));
                    }
                    bytes[jump_at - 2..jump_at].copy_from_slice(&jump.to_le_bytes());
                }
            }
            _ => {}
        }
    }
    if !blocks.is_empty() {
        return Err(Error::Malformed("unclosed Loop, While, or For block"));
    }
    Ok(())
}

fn patch_structured_else(bytes: &mut [u8], instructions: &[(usize, u8)]) -> Result<()> {
    let mut stack = Vec::new();
    for &(at, code) in instructions.iter().rev() {
        match code {
            0x1f => stack.push(0x1f),
            0x3b => stack.push(0x3b),
            0x0b => {
                if stack.last() == Some(&0x1f) {
                    bytes[at] = 0x87;
                }
            }
            0x14 => {
                if stack.pop() != Some(0x1f) {
                    return Err(Error::Malformed("EndTry without matching Try"));
                }
            }
            0x10 => {
                if stack.pop() != Some(0x3b) {
                    return Err(Error::Malformed("EndIf without matching If Then"));
                }
            }
            _ => {}
        }
    }
    if stack.iter().any(|code| matches!(code, 0x1f | 0x3b)) {
        return Err(Error::Malformed("unclosed Try or If Then block"));
    }
    Ok(())
}

pub fn tokenize(source: &[u8], kind: VariableType, tokenized: bool) -> Result<Vec<u8>> {
    match kind {
        VariableType::Text => {
            let mut out = vec![0, 1];
            out.extend(source);
            out.extend([0, 0xe0]);
            Ok(out)
        }
        VariableType::Function if !function_is_multiline(source) => {
            // The VB implementation always tokenizes one-line functions,
            // including when its Tokenize argument is false.
            tokenized_one_line_function(source)
        }
        VariableType::Program | VariableType::Function if !tokenized => {
            let marker = if matches!(kind, VariableType::Program) {
                0x19
            } else {
                0x17
            };
            let mut out = source.to_vec();
            out.extend([0, 0, 0, marker, 0xe4, 0xe5, 0, 0, 8, 0xdc]);
            Ok(out)
        }
        VariableType::Program | VariableType::Function => tokenized_program(source, kind),
        VariableType::Expression
        | VariableType::List
        | VariableType::Matrix
        | VariableType::String => expression(source),
        _ => Err(Error::UnsupportedVariableType(kind.id())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn exact_addition() {
        assert_eq!(
            tokenize(b"1+2", VariableType::Expression, true).unwrap(),
            [0xe9, 1, 1, 0x1f, 2, 1, 0x1f, 0x8b]
        );
    }
    #[test]
    fn implicit_multiplication_matches_vb_normalization_bytes() {
        let cases: &[(&str, &[u8])] = &[
            ("2x", &[0xe9, 2, 1, 0x1f, 0x08, 0x8f]),
            ("2(x+1)", &[0xe9, 2, 1, 0x1f, 0x08, 1, 1, 0x1f, 0x8b, 0x8f]),
            (
                "(x+1)(x-1)",
                &[0xe9, 0x08, 1, 1, 0x1f, 0x8b, 0x08, 1, 1, 0x1f, 0x8d, 0x8f],
            ),
            ("πx", &[0xe9, 0x24, 0x08, 0x8f]),
        ];
        for (source, expected) in cases {
            let source = crate::charset::encode_source(source).unwrap();
            assert_eq!(
                tokenize(&source, VariableType::Expression, true).unwrap(),
                *expected
            );
        }
    }

    #[test]
    fn generic_conversion_and_e_power_use_vb_tokens() {
        for (source, expected) in [
            ("x⇥y", vec![0xe9, 0x08, 0x09, 0x05, 0xe3]),
            (
                "foo⇥bar",
                vec![
                    0xe9, 0, b'f', b'o', b'o', 0, 0, b'b', b'a', b'r', 0, 0x05, 0xe3,
                ],
            ),
            ("ℯ^(x)", vec![0xe9, 0x08, 0x52]),
        ] {
            let source = crate::charset::encode_source(source).unwrap();
            assert_eq!(
                tokenize(&source, VariableType::Expression, true).unwrap(),
                expected
            );
        }
    }

    #[test]
    fn contextual_instruction_layouts_are_vb_derived() {
        let cases: &[(&[u8], &[u8])] = &[
            (b"BldData x", &[0x08, 0x90, 0xe4]),
            (b"DrwCtour x", &[0x08, 0x91, 0xe4]),
            (b"NewProb", &[0x92, 0xe4]),
            (b"SendChat x", &[0x08, 0x97, 0xe4]),
            (b"LineTan 1", &[0x2e, 1, 1, 0x1f, 0x3e, 0xe4]),
            (b"LineTan ,2", &[2, 1, 0x1f, 0x2e, 0x3e, 0xe4]),
            (b"Disp 1;2", &[0xe5, 2, 1, 0x1f, 1, 1, 0x1f, 0x7a, 0xe4]),
            (
                b"Define f(x)=x+1",
                &[
                    0x08, 0xf0, 1, 1, 0x1f, 0x8b, 0xe5, 0x08, 0x10, 0xda, 0x86, 0xe4,
                ],
            ),
        ];
        for (source, expected) in cases {
            let (actual, _) = instruction(source, None).unwrap().unwrap();
            assert_eq!(&actual, expected, "{}", String::from_utf8_lossy(source));
        }
    }

    #[test]
    fn elseif_then_round_trips_with_then_keyword() {
        let source = b"()\rPrgm\rIf x Then\rElseIf y Then\rEndIf\rEndPrgm";
        let payload = tokenize(source, VariableType::Program, true).unwrap();
        assert!(payload.windows(2).any(|window| window == [0x39, 0xe4]));
        assert_eq!(crate::detokenize::detokenize(&payload).unwrap(), source);
    }

    #[test]
    fn define_round_trips_and_marks_only_the_body_parameter_local() {
        let source = b"()\rPrgm\rDefine f(x)=x+1\rEndPrgm";
        let payload = tokenize(source, VariableType::Program, true).unwrap();
        let define = payload
            .windows(12)
            .find(|window| window.ends_with(&[0x86, 0xe4]))
            .unwrap();
        assert_eq!(
            define,
            [0x08, 0xf0, 1, 1, 0x1f, 0x8b, 0xe5, 0x08, 0x10, 0xda, 0x86, 0xe4,]
        );
        assert_eq!(crate::detokenize::detokenize(&payload).unwrap(), source);
    }
    #[test]
    fn every_ordinary_binary_operator_has_the_vb_operand_order() {
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
            let source = crate::charset::encode_source(source).unwrap();
            assert_eq!(
                tokenize(&source, VariableType::Expression, true).unwrap(),
                [0xe9, 0x08, 0x09, token]
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
            let source = crate::charset::encode_source(source).unwrap();
            assert_eq!(
                tokenize(&source, VariableType::Expression, true).unwrap(),
                [0xe9, 0x09, 0x08, token]
            );
        }
    }
    #[test]
    fn store_uses_calculator_operand_order() {
        let source = crate::charset::encode_source("v→x").unwrap();
        assert_eq!(
            tokenize(&source, VariableType::Expression, true).unwrap(),
            [0xe9, 0x06, 0x08, 0x80]
        );
    }
    #[test]
    fn concatenation_uses_calculator_operand_order() {
        assert_eq!(
            tokenize(b"\"left\"&s", VariableType::Expression, true).unwrap(),
            [0xe9, 0, b'l', b'e', b'f', b't', 0, 0x2d, 0x03, 0x14, 0xe3]
        );
    }
    #[test]
    fn extended_binary_forms_use_source_operand_order() {
        let unit = crate::charset::encode_source("x⇥_m").unwrap();
        assert_eq!(
            tokenize(&unit, VariableType::Expression, true).unwrap(),
            [0xe9, 0x08, 0, b'_', b'm', 0, 0x05, 0xe3]
        );

        let angle = crate::charset::encode_source("x∠y").unwrap();
        assert_eq!(
            tokenize(&angle, VariableType::Expression, true).unwrap(),
            [0xe9, 0x08, 0x09, 0x26, 0xe3]
        );
    }

    #[test]
    fn vb_scan_direction_controls_equal_precedence_associativity() {
        assert_eq!(
            tokenize(b"x-y-z", VariableType::Expression, true).unwrap(),
            [0xe9, 0x08, 0x09, 0x8d, 0x0a, 0x8d]
        );
        assert_eq!(
            tokenize(b"x^y^z", VariableType::Expression, true).unwrap(),
            [0xe9, 0x0a, 0x09, 0x93, 0x08, 0x93]
        );
        assert_eq!(
            tokenize(b"x&y&z", VariableType::Expression, true).unwrap(),
            [0xe9, 0x08, 0x09, 0x0a, 0x14, 0xe3, 0x14, 0xe3]
        );
    }
    #[test]
    fn text_wrapper() {
        assert_eq!(
            tokenize(b"hi", VariableType::Text, true).unwrap(),
            [0, 1, b'h', b'i', 0, 0xe0]
        );
    }
    #[test]
    fn list_vector() {
        let tokenized = tokenize(b"{1,2}", VariableType::List, true).unwrap();
        assert_eq!(tokenized, [0xe9, 0xe5, 2, 1, 0x1f, 1, 1, 0x1f, 0xd9]);
        assert_eq!(crate::detokenize::detokenize(&tokenized).unwrap(), b"{1,2}");
    }
    #[test]
    fn nested_program_branches_are_patched() {
        let source = b"()\rPrgm\rLoop\rWhile x<3\rCycle\rExit\rEndWhile\rEndLoop\rEndPrgm";
        let payload = tokenize(source, VariableType::Program, true).unwrap();
        for window in payload
            .windows(4)
            .filter(|w| w[3] == 0xe4 && matches!(w[2], 0x07 | 0x0e | 0x11 | 0x15 | 0x16))
        {
            assert_ne!(&window[..2], &[0, 0]);
        }
        let position = |code| payload.windows(2).position(|w| w == [code, 0xe4]).unwrap();
        let end = position(0x11);
        let inner_end = position(0x15);
        let open = position(0x18);
        let cycle = position(0x07);
        let exit = position(0x16);
        let stored = |at| u16::from_le_bytes([payload[at - 2], payload[at - 1]]) as usize;
        assert_eq!(stored(end), open.abs_diff(end) + 2);
        assert_eq!(stored(cycle), cycle.abs_diff(inner_end) - 2);
        assert_eq!(stored(exit), exit.abs_diff(inner_end) + 2);
        assert_eq!(crate::detokenize::detokenize(&payload).unwrap(), source);
    }
    #[test]
    fn numeric_payload_bytes_are_not_mistaken_for_branch_instructions() {
        // 6372 is 0x18e4, which embeds the Loop opcode and instruction delimiter
        // in an integer magnitude. Branch patching must follow parsed statements,
        // not scan arbitrary payload bytes for that pair.
        let source = b"()\rPrgm\rLoop\rDisp 6372\rEndLoop\rEndPrgm";
        let payload = tokenize(source, VariableType::Program, true).unwrap();
        assert_eq!(crate::detokenize::detokenize(&payload).unwrap(), source);
    }
    #[test]
    fn builtins_and_calculator_variables() {
        let source = crate::charset::encode_source("solve(x=1,x)+π+α").unwrap();
        let payload = tokenize(&source, VariableType::Expression, true).unwrap();
        assert_eq!(
            crate::charset::decode_source(&crate::detokenize::detokenize(&payload).unwrap()),
            "solve(x=1,x)+π+α"
        );
    }
    #[test]
    fn numeric_and_postfix_regressions() {
        for source in ["3/4", "0b11011", "0hf7e4", "x[1]", "5!", "mᵀ"] {
            let bytes = crate::charset::encode_source(source).unwrap();
            let payload = tokenize(&bytes, VariableType::Expression, true).unwrap();
            let decoded =
                crate::charset::decode_source(&crate::detokenize::detokenize(&payload).unwrap());
            assert_eq!(decoded, source);
        }
    }
    #[test]
    fn system_extended_and_user_functions() {
        for source in [
            "xmin+regCoef[1]",
            "getDate()+getTime()",
            "f(.5)",
            "fracs\\f4(x)",
        ] {
            let bytes = crate::charset::encode_source(source).unwrap();
            let payload = tokenize(&bytes, VariableType::Expression, true).unwrap();
            let expected = if source == "1ᴇ20+.5" {
                "1.ᴇ20+.5"
            } else {
                source
            };
            assert_eq!(
                crate::charset::decode_source(&crate::detokenize::detokenize(&payload).unwrap()),
                expected
            );
        }
    }
    #[test]
    fn dms_inverse_and_vectors() {
        for source in ["12°34'56\"", "sin⁻¹(x)", "[3,∠π/4]", "[1,2;3,4]"] {
            let bytes = crate::charset::encode_source(source).unwrap();
            let payload = tokenize(&bytes, VariableType::Expression, true).unwrap();
            let expected = if source == "[1,2;3,4]" {
                "[[1,2][3,4]]"
            } else {
                source
            };
            assert_eq!(
                crate::charset::decode_source(&crate::detokenize::detokenize(&payload).unwrap()),
                expected
            );
        }
    }
    #[test]
    fn local_markers_follow_arguments_and_declarations() {
        let source = b"(x)\rFunc\r  Local y\r  y+x\rEndFunc";
        let payload = tokenize(source, VariableType::Function, true).unwrap();
        assert!(payload.windows(2).any(|w| w[1] == 0xf0));
        assert_eq!(crate::detokenize::detokenize(&payload).unwrap(), source);
    }

    #[test]
    fn non_ascii_arguments_and_declarations_receive_local_markers() {
        let source = b"(\x91)\rFunc\rLocal \x90\rReturn [[cos(\x91),\x90]]\rEndFunc";
        let payload = tokenize(source, VariableType::Function, true).unwrap();
        assert!(payload.windows(4).any(|bytes| bytes == [0, 0x91, 0, 0xf0]));
        assert!(payload.windows(4).any(|bytes| bytes == [0, 0x90, 0, 0xf0]));
    }

    #[test]
    fn graphlink_transpose_escape_is_a_postfix_not_part_of_a_variable() {
        let payload = tokenize(b"iri1\x99", VariableType::Expression, true).unwrap();
        assert_eq!(
            crate::detokenize::detokenize(&payload).unwrap(),
            b"iri1\x99"
        );
        assert!(payload.ends_with(&[0x75]));
    }

    #[test]
    fn graphlink_folder_dots_are_preserved_in_qualified_names() {
        let payload = tokenize(b"tistat.fcdf(x,1,2)", VariableType::Expression, true).unwrap();
        assert!(payload.windows(13).any(|bytes| bytes == b"\0tistat.fcdf\0"));
        assert_eq!(
            crate::detokenize::detokenize(&payload).unwrap(),
            b"tistat.fcdf(x,1,2)"
        );
    }
    #[test]
    fn arbitrary_boolean_and_plus_minus_tokens() {
        for source in ["@3+@n4", "±x", "x±y", "f(,x)", "undef or true"] {
            let bytes = crate::charset::encode_source(source).unwrap();
            let payload = tokenize(&bytes, VariableType::Expression, true).unwrap();
            assert_eq!(
                crate::charset::decode_source(&crate::detokenize::detokenize(&payload).unwrap()),
                source
            );
        }
    }
    #[test]
    fn late_special_function_and_operator_tokens() {
        for source in [
            "eigVc(m)",
            "deSolve(y'=y,t,y)",
            "isPrime(7)",
            "rotate({1,2},1)",
            "x|x>0",
            "\"a\"&\"b\"",
            "f'(x)",
            "x'",
            "∞",
            "−∞",
        ] {
            let bytes = crate::charset::encode_source(source).unwrap();
            let payload = tokenize(&bytes, VariableType::Expression, true).unwrap();
            assert_eq!(
                crate::charset::decode_source(&crate::detokenize::detokenize(&payload).unwrap()),
                source
            );
        }
    }
    #[test]
    fn colon_statements_preserve_strings_comments_and_spacing() {
        let source = b"()\rPrgm\r  Disp \"a:b\": Disp 2 \xa9 c:d\rEndPrgm";
        let payload = tokenize(source, VariableType::Program, true).unwrap();
        assert_eq!(crate::detokenize::detokenize(&payload).unwrap(), source);
    }
    #[test]
    fn conversion_targets_use_extended_tokens() {
        for source in ["255⇥Hex", "x⇥DMS", "x⇥Polar", "x⇥Rad"] {
            let bytes = crate::charset::encode_source(source).unwrap();
            let payload = tokenize(&bytes, VariableType::Expression, true).unwrap();
            assert_eq!(
                crate::charset::decode_source(&crate::detokenize::detokenize(&payload).unwrap()),
                source
            );
        }
    }
    #[test]
    fn try_else_uses_try_specific_instruction_token() {
        let source = b"()\rPrgm\rTry\r  Disp 1\rElse\r  Disp 2\rEndTry\rEndPrgm";
        let payload = tokenize(source, VariableType::Program, true).unwrap();
        assert!(payload.windows(2).any(|w| w == [0x87, 0xe4]));
        assert_eq!(crate::detokenize::detokenize(&payload).unwrap(), source);
    }
    #[test]
    fn indirection_and_custom_units_use_extended_operators() {
        for source in ["#x", "3⇥_m"] {
            let bytes = crate::charset::encode_source(source).unwrap();
            let payload = tokenize(&bytes, VariableType::Expression, true).unwrap();
            assert_eq!(
                crate::charset::decode_source(&crate::detokenize::detokenize(&payload).unwrap()),
                source
            );
        }
    }
    #[test]
    fn conversion_named_functions_scientific_numbers_and_quoted_quotes() {
        for source in [
            "list⇥mat({1,2},1)",
            "mat⇥list([[1,2]])",
            "exp⇥list(x,2)",
            "1ᴇ20+.5",
            "\"a\"\"b\"",
        ] {
            let bytes = crate::charset::encode_source(source).unwrap();
            let payload = tokenize(&bytes, VariableType::Expression, true).unwrap();
            let expected = if source == "1ᴇ20+.5" {
                "1.ᴇ20+.5"
            } else {
                source
            };
            assert_eq!(
                crate::charset::decode_source(&crate::detokenize::detokenize(&payload).unwrap()),
                expected
            );
        }
    }
    #[test]
    fn fixed_function_vectors_match_legacy_layout() {
        assert_eq!(
            tokenize(b"solve(1,2)", VariableType::Expression, true).unwrap(),
            [0xe9, 2, 1, 0x1f, 1, 1, 0x1f, 0x96]
        );
        let source = crate::charset::encode_source("list⇥mat({1,2},1)").unwrap();
        let payload = tokenize(&source, VariableType::Expression, true).unwrap();
        assert_eq!(payload.first(), Some(&0xe9));
        assert_eq!(payload.get(1), Some(&0xe5));
        assert_eq!(payload.last(), Some(&0xd3));
        assert!(!tokenize(b"1/2", VariableType::Expression, true)
            .unwrap()
            .contains(&0x21));
    }

    #[test]
    fn vb_extended_function_variants_use_their_exact_stack_layouts() {
        let vectors: &[(&str, &[u8])] = &[
            ("det(m,1)", &[0xe9, 1, 1, 0x1f, 0x17, 0x30, 0xe3]),
            ("ref(m,1)", &[0xe9, 1, 1, 0x1f, 0x17, 0x31, 0xe3]),
            ("rref(m,1)", &[0xe9, 1, 1, 0x1f, 0x17, 0x32, 0xe3]),
            ("simult(a,b,c)", &[0xe9, 0x0d, 0x0c, 0x0b, 0x33, 0xe3]),
            (
                "product(1,2,3)",
                &[0xe9, 0xe5, 3, 1, 0x1f, 2, 1, 0x1f, 1, 1, 0x1f, 0x37, 0xe3],
            ),
            (
                "sum(1,2,3)",
                &[0xe9, 0xe5, 3, 1, 0x1f, 2, 1, 0x1f, 1, 1, 0x1f, 0x39, 0xe3],
            ),
            ("root(2,3)", &[0xe9, 3, 1, 0x1f, 2, 1, 0x1f, 0x57, 0xe3]),
        ];
        for (source, expected) in vectors {
            let source = crate::charset::encode_source(source).unwrap();
            let payload = tokenize(&source, VariableType::Expression, true).unwrap();
            assert_eq!(&payload, expected, "{source:?}");
            assert_eq!(crate::detokenize::detokenize(&payload).unwrap(), source);
        }

        let delta = crate::charset::encode_source("Δlist(x)").unwrap();
        let payload = tokenize(&delta, VariableType::Expression, true).unwrap();
        assert_eq!(payload, [0xe9, 0x08, 0x3b, 0xe3]);
        assert_eq!(crate::detokenize::detokenize(&payload).unwrap(), delta);

        assert_eq!(
            tokenize(b"augment(a,b)", VariableType::Expression, true).unwrap(),
            [0xe9, 0x0c, 0x0b, 0xad]
        );
        let extended = tokenize(b"augment(a;b)", VariableType::Expression, true).unwrap();
        assert_eq!(extended, [0xe9, 0x0c, 0x0b, 0x35, 0xe3]);
        assert_eq!(
            crate::detokenize::detokenize(&extended).unwrap(),
            b"augment(a;b)"
        );
    }

    #[test]
    fn one_line_functions_are_always_tokenized_like_vb() {
        let source = b"(x)\rx+1";
        let expected = [
            0xe9, 0x08, 0xf0, 1, 1, 0x1f, 0x8b, 0xe5, 0x08, 0, 0, 0, 0xdc,
        ];
        assert_eq!(
            tokenize(source, VariableType::Function, true).unwrap(),
            expected
        );
        assert_eq!(
            tokenize(source, VariableType::Function, false).unwrap(),
            expected
        );
        assert_eq!(crate::detokenize::detokenize(&expected).unwrap(), source);
    }

    #[test]
    fn branch_instructions_outside_loops_are_rejected() {
        let cycle = b"()\rPrgm\rCycle\rEndPrgm";
        assert!(matches!(
            tokenize(cycle, VariableType::Program, true),
            Err(Error::Malformed(msg)) if msg.contains("Cycle outside a loop")
        ));

        let exit = b"()\rPrgm\rExit\rEndPrgm";
        assert!(matches!(
            tokenize(exit, VariableType::Program, true),
            Err(Error::Malformed(msg)) if msg.contains("Exit outside a loop")
        ));
    }

    #[test]
    fn mismatched_or_unclosed_loop_blocks_are_rejected() {
        let mismatched = b"()\rPrgm\rLoop\rEndWhile\rEndPrgm";
        assert!(matches!(
            tokenize(mismatched, VariableType::Program, true),
            Err(Error::Malformed(msg)) if msg.contains("mismatched loop terminator")
        ));

        let unclosed = b"()\rPrgm\rLoop\rDisp 1\rEndPrgm";
        assert!(matches!(
            tokenize(unclosed, VariableType::Program, true),
            Err(Error::Malformed(msg)) if msg.contains("unclosed Loop")
        ));
    }

    #[test]
    fn structured_if_try_terminators_are_validated() {
        let end_if_without_if_then = b"()\rPrgm\rEndIf\rEndPrgm";
        assert!(matches!(
            tokenize(end_if_without_if_then, VariableType::Program, true),
            Err(Error::Malformed(msg)) if msg.contains("EndIf without matching If Then")
        ));

        let end_try_without_try = b"()\rPrgm\rEndTry\rEndPrgm";
        assert!(matches!(
            tokenize(end_try_without_try, VariableType::Program, true),
            Err(Error::Malformed(msg)) if msg.contains("EndTry without matching Try")
        ));

        let unclosed_try = b"()\rPrgm\rTry\rDisp 1\rEndPrgm";
        assert!(matches!(
            tokenize(unclosed_try, VariableType::Program, true),
            Err(Error::Malformed(msg)) if msg.contains("unclosed Try or If Then block")
        ));
    }
}
