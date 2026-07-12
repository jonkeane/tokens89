pub const END_STACK: u8 = 0xe9;

pub fn unary_name(token: u8) -> Option<&'static str> {
    Some(match token {
        0x2f => "cosh⁻¹",
        0x30 => "sinh⁻¹",
        0x31 => "tanh⁻¹",
        0x32 => "sech⁻¹",
        0x33 => "csch⁻¹",
        0x34 => "coth⁻¹",
        0x35 => "cosh",
        0x36 => "sinh",
        0x37 => "tanh",
        0x38 => "sech",
        0x39 => "csch",
        0x3a => "coth",
        0x3b => "cos⁻¹",
        0x3c => "sin⁻¹",
        0x3d => "tan⁻¹",
        0x3e => "sec⁻¹",
        0x3f => "csc⁻¹",
        0x40 => "cot⁻¹",
        0x44 => "cos",
        0x45 => "sin",
        0x46 => "tan",
        0x47 => "sec",
        0x48 => "csc",
        0x49 => "cot",
        0x4b => "abs",
        0x4c => "angle",
        0x4d => "ceiling",
        0x4e => "floor",
        0x4f => "int",
        0x50 => "sign",
        0x51 => "√",
        0x52 => "ℯ^",
        0x53 => "ln",
        0x54 => "log",
        0x55 => "fPart",
        0x56 => "iPart",
        0x57 => "conj",
        0x58 => "imag",
        0x59 => "real",
        0x5a => "approx",
        0x5b => "tExpand",
        0x5c => "tCollect",
        0x5d => "getDenom",
        0x5e => "getNum",
        0x60 => "cumSum",
        0x61 => "det",
        0x62 => "colNorm",
        0x63 => "rowNorm",
        0x64 => "norm",
        0x65 => "mean",
        0x66 => "median",
        0x67 => "product",
        0x68 => "stdDev",
        0x69 => "sum",
        0x6a => "variance",
        0x6b => "unitV",
        0x6c => "dim",
        0x6d => "mat⇥list",
        0x6e => "newList",
        0x6f => "rref",
        0x70 => "ref",
        0x71 => "identity",
        0x72 => "diag",
        0x73 => "colDim",
        0x74 => "rowDim",
        0x79 => "not",
        0xed => "eigVc",
        0xee => "eigVl",
        0xf4 => "isPrime",
        _ => return None,
    })
}

pub fn builtin_name(token: u8) -> Option<&'static str> {
    Some(match token {
        0x96 => "solve",
        0x97 => "cSolve",
        0x98 => "nSolve",
        0x99 => "zeros",
        0x9a => "cZeros",
        0x9b => "fMin",
        0x9c => "fMax",
        0x9e => "polyEval",
        0x9f => "randPoly",
        0xa0 => "crossP",
        0xa1 => "dotP",
        0xa2 => "gcd",
        0xa3 => "lcm",
        0xa4 => "mod",
        0xa5 => "intDiv",
        0xa6 => "remain",
        0xa7 => "nCr",
        0xa8 => "nPr",
        0xa9 => "P⇥Rx",
        0xaa => "P⇥Ry",
        0xab => "R⇥Pθ",
        0xac => "R⇥Pr",
        0xad => "augment",
        0xae => "newMat",
        0xaf => "randMat",
        0xb0 => "simult",
        0xb1 => "part",
        0xb2 => "exp⇥list",
        0xb3 => "randNorm",
        0xb4 => "mRow",
        0xb5 => "rowAdd",
        0xb6 => "rowSwap",
        0xb7 => "arcLen",
        0xb8 => "nInt",
        0xb9 => "Π",
        0xba => "Σ",
        0xbb => "nInt",
        0xbc => "ans",
        0xbd => "entry",
        0xbe => "exact",
        0xc0 => "comDenom",
        0xc1 => "expand",
        0xc2 => "factor",
        0xc3 => "cFactor",
        0xc4 => "∫",
        0xc5 => "∂",
        0xc6 => "avgRC",
        0xc7 => "nDeriv",
        0xc8 => "taylor",
        0xc9 => "limit",
        0xca => "propFrac",
        0xcb => "when",
        0xcc => "round",
        0xce => "left",
        0xcf => "right",
        0xd0 => "mid",
        0xd1 => "shift",
        0xd2 => "seq",
        0xd3 => "list⇥mat",
        0xd4 => "subMat",
        0xd6 => "rand",
        0xd7 => "min",
        0xd8 => "max",
        0xf1 => "deSolve",
        0xf9 => "rotate",
        _ => return None,
    })
}
pub fn builtin_token(name: &str) -> Option<u8> {
    let n = name.to_ascii_lowercase();
    if n == "mrowadd" {
        return Some(0xbb);
    }
    (0u8..=255).find(|&t| builtin_name(t).is_some_and(|v| v.to_ascii_lowercase() == n))
}
pub fn builtin_arity(token: u8) -> Option<usize> {
    match token {
        0x96..=0xb0 | 0xb2 | 0xb3 => Some(2),
        0xed | 0xee | 0xf4 => Some(1),
        _ => None,
    }
}
pub fn unary_token(name: &str) -> Option<u8> {
    let lower = name.to_ascii_lowercase();
    if lower == "sqrt" {
        return Some(0x51);
    }
    (0u8..=255).find(|&t| unary_name(t).is_some_and(|n| n.to_ascii_lowercase() == lower))
}
pub fn binary_op(token: u8) -> Option<&'static str> {
    Some(match token {
        0x80 => "→",
        0x81 => "|",
        0x82 => " xor ",
        0x83 => " or ",
        0x84 => " and ",
        0x85 => "<",
        0x86 => "≤",
        0x87 => "=",
        0x88 => "≥",
        0x89 => ">",
        0x8a => "≠",
        0x8b => "+",
        0x8c => ".+",
        0x8d => "-",
        0x8e => ".-",
        0x8f => "*",
        0x90 => ".*",
        0x91 => "/",
        0x92 => "./",
        0x93 => "^",
        0x94 => ".^",
        0xeb => "±",
        _ => return None,
    })
}
pub fn binary_token(op: &str) -> Option<u8> {
    Some(match op {
        "→" | "->" => 0x80,
        "|" => 0x81,
        "xor" => 0x82,
        "or" => 0x83,
        "and" => 0x84,
        "<" => 0x85,
        "≤" | "<=" => 0x86,
        "=" => 0x87,
        "≥" | ">=" => 0x88,
        ">" => 0x89,
        "≠" | "!=" | "/=" => 0x8a,
        "+" => 0x8b,
        ".+" => 0x8c,
        "-" => 0x8d,
        ".-" => 0x8e,
        "*" => 0x8f,
        ".*" => 0x90,
        "/" => 0x91,
        "./" => 0x92,
        "^" => 0x93,
        ".^" => 0x94,
        "±" => 0xeb,
        _ => return None,
    })
}

/// Whether AMS stores this operator's operands in source order instead of the
/// reverse order used by the other binary operators.
pub fn binary_operands_are_in_source_order(token: u8) -> bool {
    matches!(token, 0x80 | 0x8b..=0x92 | 0xeb)
}

pub fn precedence(op: &str) -> u8 {
    match op {
        "→" | "->" | "|" | "∠" => 1,
        "or" | "xor" => 2,
        "and" => 3,
        "=" | "<" | ">" | "<=" | ">=" | "≤" | "≥" | "!=" | "≠" | "/=" | "&" => 4,
        "+" | "-" | ".+" | ".-" | "±" => 5,
        "*" | "/" | ".*" | "./" => 6,
        "^" | ".^" => 7,
        _ => 0,
    }
}

pub fn instruction_name(code: u8) -> Option<&'static str> {
    Some(match code {
        0x01 => "ClrDraw",
        0x02 => "ClrGraph",
        0x03 => "ClrHome",
        0x04 => "ClrIO",
        0x05 => "ClrTable",
        0x06 => "Custom",
        0x07 => "Cycle",
        0x08 => "Dialog",
        0x09 => "DispG",
        0x0a => "DispTbl",
        0x0b => "Else",
        0x0c => "EndCustm",
        0x0d => "EndDlog",
        0x0e => "EndFor",
        0x0f => "EndFunc",
        0x10 => "EndIf",
        0x11 => "EndLoop",
        0x12 => "EndPrgm",
        0x13 => "EndTBar",
        0x14 => "EndTry",
        0x15 => "EndWhile",
        0x16 => "Exit",
        0x17 => "Func",
        0x18 => "Loop",
        0x19 => "Prgm",
        0x1a => "ShowStat",
        0x1b => "Stop",
        0x1c => "Then",
        0x1d => "Toolbar",
        0x1e => "Trace",
        0x1f => "Try",
        0x20 => "ZoomBox",
        0x21 => "ZoomData",
        0x22 => "ZoomDec",
        0x23 => "ZoomFit",
        0x24 => "ZoomIn",
        0x25 => "ZoomInt",
        0x26 => "ZoomOut",
        0x27 => "ZoomPrev",
        0x28 => "ZoomRcl",
        0x29 => "ZoomSqr",
        0x2a => "ZoomStd",
        0x2b => "ZoomSto",
        0x2c => "ZoomTrig",
        0x2d => "DrawFunc",
        0x2e => "DrawInv",
        0x2f => "Goto",
        0x30 => "Lbl",
        0x31 => "Get",
        0x32 => "Send",
        0x33 => "GetCalc",
        0x34 => "SendCalc",
        0x35 => "NewFold",
        0x36 => "PrintObj",
        0x37 => "RclGDB",
        0x38 => "StoGDB",
        0x39 => "ElseIf",
        0x3a => "If",
        0x3b => "If",
        0x3c => "RandSeed",
        0x3d => "While",
        0x3e => "LineTan",
        0x3f => "CopyVar",
        0x40 => "Rename",
        0x41 => "Style",
        0x42 => "Fill",
        0x43 | 0x99 => "Request",
        0x44 => "PopUp",
        0x45 => "PtChg",
        0x46 => "PtOff",
        0x47 => "PtOn",
        0x48 => "PxlChg",
        0x49 => "PxlOff",
        0x4a => "PxlOn",
        0x4b => "MoveVar",
        0x4c => "DropDown",
        0x4d => "Output",
        0x4e => "PtText",
        0x4f => "PxlText",
        0x50 => "DrawSlp",
        0x51 => "Pause",
        0x52 => "Return",
        0x53 => "Input",
        0x54 => "PlotsOff",
        0x55 => "PlotsOn",
        0x56 => "Title",
        0x57 => "Item",
        0x58 => "InputStr",
        0x59 => "LineHorz",
        0x5a => "LineVert",
        0x5b => "PxlHorz",
        0x5c => "PxlVert",
        0x5d => "AndPic",
        0x5e => "RclPic",
        0x5f => "RplcPic",
        0x60 => "XorPic",
        0x61 => "DrawPol",
        0x62 => "Text",
        0x63 => "OneVar",
        0x64 => "StoPic",
        0x65 => "Graph",
        0x66 => "Table",
        0x67 => "NewPic",
        0x68 => "DrawParm",
        0x69 => "CyclePic",
        0x6a => "CubicReg",
        0x6b => "ExpReg",
        0x6c => "LinReg",
        0x6d => "LnReg",
        0x6e => "MedMed",
        0x6f => "PowerReg",
        0x70 => "QuadReg",
        0x71 => "QuartReg",
        0x72 => "TwoVar",
        0x73 => "Shade",
        0x74 => "For",
        0x75 => "Circle",
        0x76 => "PxlCrcl",
        0x77 => "NewPlot",
        0x78 => "Line",
        0x79 => "PxlLine",
        0x7a => "Disp",
        0x7b => "FnOff",
        0x7c => "FnOn",
        0x7d => "Local",
        0x7e => "DelFold",
        0x7f => "DelVar",
        0x80 => "Lock",
        0x81 => "Prompt",
        0x82 => "SortA",
        0x83 => "SortD",
        0x84 => "UnLock",
        0x85 => "NewData",
        0x86 => "Define",
        0x87 => "Else",
        0x88 => "ClrErr",
        0x89 => "PassErr",
        0x8a => "DispHome",
        0x8b => "Exec",
        0x8c => "Archive",
        0x8d => "Unarchiv",
        0x8e => "LU",
        0x8f => "QR",
        0x90 => "BldData",
        0x91 => "DrwCtour",
        0x92 => "NewProb",
        0x93 => "SinReg",
        0x94 => "Logistic",
        0x95 => "CustmOn",
        0x96 => "CustmOff",
        0x97 => "SendChat",
        0x9a => "ClockOn",
        0x9b => "ClockOff",
        _ => return None,
    })
}

pub fn instruction_token(name: &str, has_then: bool) -> Option<u8> {
    let n = name.to_ascii_lowercase();
    if n == "if" && has_then {
        return Some(0x3b);
    }
    (1..=0x9b).find(|&code| instruction_name(code).is_some_and(|v| v.to_ascii_lowercase() == n))
}

pub fn instruction_arity(code: u8) -> Option<usize> {
    match code {
        0x2d..=0x3d => Some(1),
        0x3e..=0x4a => Some(2),
        0x4b..=0x50 => Some(3),
        0x86 => Some(2),
        0x90 | 0x91 | 0x97 => Some(1),
        0x51..=0x85 | 0x8b..=0x8f | 0x93 | 0x94 | 0x99 => None,
        _ => Some(0),
    }
}

pub fn has_displacement(code: u8) -> bool {
    matches!(code, 0x07 | 0x0e | 0x11 | 0x15 | 0x16)
}

pub fn system_variable_name(code: u8) -> Option<&'static str> {
    Some(match code {
        0x01 => "ẋ",
        0x02 => "ẏ",
        0x03 => "Σx",
        0x04 => "Σx²",
        0x05 => "Σy",
        0x06 => "Σy²",
        0x07 => "Σxy",
        0x08 => "Sx",
        0x09 => "Sy",
        0x0a => "σx",
        0x0b => "σy",
        0x0c => "nStat",
        0x0d => "minX",
        0x0e => "minY",
        0x0f => "q1",
        0x10 => "medStat",
        0x11 => "q3",
        0x12 => "maxX",
        0x13 => "maxY",
        0x14 => "corr",
        0x15 => "R²",
        0x16 => "medx1",
        0x17 => "medx2",
        0x18 => "medx3",
        0x19 => "medy1",
        0x1a => "medy2",
        0x1b => "medy3",
        0x1c => "xc",
        0x1d => "yc",
        0x1e => "zc",
        0x1f => "tc",
        0x20 => "rc",
        0x21 => "θc",
        0x22 => "nc",
        0x23 => "xfact",
        0x24 => "yfact",
        0x25 => "zfact",
        0x26 => "xmin",
        0x27 => "xmax",
        0x28 => "xscl",
        0x29 => "ymin",
        0x2a => "ymax",
        0x2b => "yscl",
        0x2c => "Δx",
        0x2d => "Δy",
        0x2e => "xres",
        0x2f => "xgrid",
        0x30 => "ygrid",
        0x31 => "zmin",
        0x32 => "zmax",
        0x33 => "zscl",
        0x34 => "eyeθ",
        0x35 => "eyeφ",
        0x36 => "θmin",
        0x37 => "θmax",
        0x38 => "θstep",
        0x39 => "tmin",
        0x3a => "tmax",
        0x3b => "tstep",
        0x3c => "nmin",
        0x3d => "nmax",
        0x3e => "plotStrt",
        0x3f => "plotStep",
        0x40 => "zxmin",
        0x41 => "zxmax",
        0x42 => "zxscl",
        0x43 => "zymin",
        0x44 => "zymax",
        0x45 => "zyscl",
        0x46 => "zxres",
        0x47 => "zθmin",
        0x48 => "zθmax",
        0x49 => "zθstep",
        0x4a => "ztmin",
        0x4b => "ztmax",
        0x4c => "ztstep",
        0x4d => "zxgrid",
        0x4e => "zygrid",
        0x4f => "zzmin",
        0x50 => "zzmax",
        0x51 => "zzscl",
        0x52 => "zeyeθ",
        0x53 => "zeyeφ",
        0x54 => "znmin",
        0x55 => "znmax",
        0x56 => "zpltstep",
        0x57 => "zpltstrt",
        0x58 => "seed1",
        0x59 => "seed2",
        0x5a => "ok",
        0x5b => "errornum",
        0x5c => "sysMath",
        0x5d => "sysData",
        0x5f => "regCoef",
        0x60 => "tblInput",
        0x61 => "tblStart",
        0x62 => "Δtbl",
        0x64 => "eyeψ",
        0x65 => "tplot",
        0x66 => "diftol",
        0x67 => "zeyeψ",
        0x68 => "t0",
        0x69 => "dtime",
        0x6a => "ncurves",
        0x6b => "fldres",
        0x6c => "Estep",
        0x6d => "zt0de",
        0x6e => "ztmaxde",
        0x6f => "ztstepde",
        0x70 => "ztplotde",
        0x71 => "ncontour",
        _ => return None,
    })
}
pub fn system_variable_token(name: &str) -> Option<u8> {
    let n = name.to_lowercase();
    if n == "zpltstrt" {
        return Some(0x56);
    }
    if n == "zpltstep" {
        return Some(0x57);
    }
    (1..=0x71).find(|&t| system_variable_name(t).is_some_and(|v| v.to_lowercase() == n))
}

pub fn extended_name(code: u8) -> Option<&'static str> {
    Some(match code {
        0x01 => "#",
        0x02 => "getKey",
        0x03 => "getFold",
        0x04 => "switch",
        0x06 => "ord",
        0x07 => "expr",
        0x08 => "char",
        0x09 => "string",
        0x0a => "getType",
        0x0b => "getMode",
        0x0c => "setFold",
        0x0d => "ptTest",
        0x0e => "pxlTest",
        0x0f => "setGraph",
        0x10 => "setTable",
        0x11 => "setMode",
        0x12 => "format",
        0x13 => "inString",
        0x27 => "tmpCnv",
        0x28 => "ΔtmpCnv",
        0x29 => "getUnits",
        0x2a => "setUnits",
        0x30 => "det",
        0x31 => "ref",
        0x32 => "rref",
        0x33 => "simult",
        0x34 => "getConfg",
        0x35 => "augment",
        0x36 => "mean",
        0x37 => "product",
        0x38 => "stdDev",
        0x39 => "sum",
        0x3a => "variance",
        0x3b => "Δlist",
        0x46 => "isClkOn",
        0x47 => "getDate",
        0x48 => "getTime",
        0x49 => "getTmZn",
        0x4a => "setDate",
        0x4b => "setTime",
        0x4c => "setTmZn",
        0x4d => "dayOfWk",
        0x4e => "startTmr",
        0x4f => "checkTmr",
        0x50 => "timeCnv",
        0x51 => "getDtFmt",
        0x52 => "getTmFmt",
        0x53 => "getDtStr",
        0x54 => "getTmStr",
        0x55 => "setDtFmt",
        0x56 => "setTmFmt",
        0x57 => "root",
        0x59 => "impDif",
        0x5b => "isVar",
        0x5c => "isLocked",
        0x5d => "isArchiv",
        _ => return None,
    })
}
pub fn extended_token(name: &str) -> Option<u8> {
    let n = name.to_lowercase();
    (1..=0x5d).find(|&t| extended_name(t).is_some_and(|v| v.to_lowercase() == n))
}
pub fn extended_arity(code: u8) -> Option<usize> {
    match code {
        0x01 | 0x06..=0x0c => Some(1),
        0x0d..=0x10 => Some(2),
        0x27 | 0x28 | 0x30..=0x32 | 0x35 | 0x36 | 0x38 | 0x3a => Some(2),
        0x33 => Some(3),
        0x3b => Some(1),
        0x2a => Some(1),
        0x4c | 0x4f | 0x50 | 0x55 | 0x56 | 0x5b..=0x5d => Some(1),
        0x57 => Some(2),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn instruction_lookup_round_trips_and_if_then_variant_is_distinct() {
        for code in 1..=0x9b {
            if let Some(name) = instruction_name(code) {
                let round = instruction_token(name, false).expect("instruction should re-map");
                assert_eq!(
                    instruction_name(round).unwrap().to_ascii_lowercase(),
                    name.to_ascii_lowercase()
                );
            }
        }
        assert_eq!(instruction_token("if", false), Some(0x3a));
        assert_eq!(instruction_token("if", true), Some(0x3b));
    }

    #[test]
    fn builtin_and_unary_lookup_paths_are_consistent() {
        for token in 0u8..=u8::MAX {
            if let Some(name) = builtin_name(token) {
                let back = builtin_token(name).expect("builtin should map back");
                assert_eq!(
                    builtin_name(back).unwrap().to_ascii_lowercase(),
                    name.to_ascii_lowercase()
                );
            }
            if let Some(name) = unary_name(token) {
                let back = unary_token(name).expect("unary should map back");
                assert_eq!(
                    unary_name(back).unwrap().to_ascii_lowercase(),
                    name.to_ascii_lowercase()
                );
            }
        }
        assert_eq!(builtin_token("mrowadd"), Some(0xbb));
        assert_eq!(unary_token("sqrt"), Some(0x51));
    }

    #[test]
    fn system_and_extended_lookup_paths_are_consistent() {
        for code in 1..=0x71 {
            if let Some(name) = system_variable_name(code) {
                let lower = name.to_lowercase();
                if lower == "zpltstep" || lower == "zpltstrt" {
                    continue;
                }
                let back = system_variable_token(name).expect("system variable should map back");
                assert_eq!(
                    system_variable_name(back).unwrap().to_lowercase(),
                    name.to_lowercase()
                );
            }
        }
        for code in 1..=0x5d {
            if let Some(name) = extended_name(code) {
                let back = extended_token(name).expect("extended token should map back");
                assert_eq!(
                    extended_name(back).unwrap().to_lowercase(),
                    name.to_lowercase()
                );
            }
        }

        // Preserve historical zplt alias behavior.
        assert_eq!(system_variable_token("zpltstrt"), Some(0x56));
        assert_eq!(system_variable_token("zpltstep"), Some(0x57));
    }
}
