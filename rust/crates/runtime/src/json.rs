use std::collections::BTreeMap;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JsonValue {
    Null,
    Bool(bool),
    Number(i64),
    String(String),
    Array(Vec<JsonValue>),
    Object(BTreeMap<String, JsonValue>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JsonError {
    message: String,
}

impl JsonError {
    #[must_use]
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl Display for JsonError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for JsonError {}

impl JsonValue {
    #[must_use]
    pub fn render(&self) -> String {
        match self {
            Self::Null => "null".to_string(),
            Self::Bool(value) => value.to_string(),
            Self::Number(value) => value.to_string(),
            Self::String(value) => render_string(value),
            Self::Array(values) => {
                let rendered = values
                    .iter()
                    .map(Self::render)
                    .collect::<Vec<_>>()
                    .join(",");
                format!("[{rendered}]")
            }
            Self::Object(entries) => {
                let rendered = entries
                    .iter()
                    .map(|(key, value)| format!("{}:{}", render_string(key), value.render()))
                    .collect::<Vec<_>>()
                    .join(",");
                format!("{{{rendered}}}")
            }
        }
    }

    pub fn parse(source: &str) -> Result<Self, JsonError> {
        let mut parser = Parser::new(source);
        let value = parser.parse_value()?;
        parser.skip_whitespace();
        if parser.is_eof() {
            Ok(value)
        } else {
            Err(JsonError::new("unexpected trailing content"))
        }
    }

    #[must_use]
    pub fn as_object(&self) -> Option<&BTreeMap<String, JsonValue>> {
        match self {
            Self::Object(value) => Some(value),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_array(&self) -> Option<&[JsonValue]> {
        match self {
            Self::Array(value) => Some(value),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Self::String(value) => Some(value),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Self::Bool(value) => Some(*value),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Self::Number(value) => Some(*value),
            _ => None,
        }
    }
}

fn render_string(value: &str) -> String {
    let mut rendered = String::with_capacity(value.len() + 2);
    rendered.push('"');
    for ch in value.chars() {
        match ch {
            '"' => rendered.push_str("\\\""),
            '\\' => rendered.push_str("\\\\"),
            '\n' => rendered.push_str("\\n"),
            '\r' => rendered.push_str("\\r"),
            '\t' => rendered.push_str("\\t"),
            '\u{08}' => rendered.push_str("\\b"),
            '\u{0C}' => rendered.push_str("\\f"),
            control if control.is_control() => push_unicode_escape(&mut rendered, control),
            plain => rendered.push(plain),
        }
    }
    rendered.push('"');
    rendered
}

fn push_unicode_escape(rendered: &mut String, control: char) {
    const HEX: &[u8; 16] = b"0123456789abcdef";

    rendered.push_str("\\u");
    let value = u32::from(control);
    for shift in [12_u32, 8, 4, 0] {
        let nibble = ((value >> shift) & 0xF) as usize;
        rendered.push(char::from(HEX[nibble]));
    }
}

struct Parser<'a> {
    chars: Vec<char>,
    index: usize,
    _source: &'a str,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().collect(),
            index: 0,
            _source: source,
        }
    }

    fn parse_value(&mut self) -> Result<JsonValue, JsonError> {
        self.skip_whitespace();
        match self.peek() {
            Some('n') => self.parse_literal("null", JsonValue::Null),
            Some('t') => self.parse_literal("true", JsonValue::Bool(true)),
            Some('f') => self.parse_literal("false", JsonValue::Bool(false)),
            Some('"') => self.parse_string().map(JsonValue::String),
            Some('[') => self.parse_array(),
            Some('{') => self.parse_object(),
            Some('-' | '0'..='9') => self.parse_number().map(JsonValue::Number),
            Some(other) => Err(JsonError::new(format!("unexpected character: {other}"))),
            None => Err(JsonError::new("unexpected end of input")),
        }
    }

    fn parse_literal(&mut self, expected: &str, value: JsonValue) -> Result<JsonValue, JsonError> {
        for expected_char in expected.chars() {
            if self.next() != Some(expected_char) {
                return Err(JsonError::new(format!(
                    "invalid literal: expected {expected}"
                )));
            }
        }
        Ok(value)
    }

    fn parse_string(&mut self) -> Result<String, JsonError> {
        self.expect('"')?;
        let mut value = String::new();
        while let Some(ch) = self.next() {
            match ch {
                '"' => return Ok(value),
                '\\' => value.push(self.parse_escape()?),
                plain => value.push(plain),
            }
        }
        Err(JsonError::new("unterminated string"))
    }

    fn parse_escape(&mut self) -> Result<char, JsonError> {
        match self.next() {
            Some('"') => Ok('"'),
            Some('\\') => Ok('\\'),
            Some('/') => Ok('/'),
            Some('b') => Ok('\u{08}'),
            Some('f') => Ok('\u{0C}'),
            Some('n') => Ok('\n'),
            Some('r') => Ok('\r'),
            Some('t') => Ok('\t'),
            Some('u') => self.parse_unicode_escape(),
            Some(other) => Err(JsonError::new(format!("invalid escape sequence: {other}"))),
            None => Err(JsonError::new("unexpected end of input in escape sequence")),
        }
    }

    fn parse_unicode_escape(&mut self) -> Result<char, JsonError> {
        let mut value = 0_u32;
        for _ in 0..4 {
            let Some(ch) = self.next() else {
                return Err(JsonError::new("unexpected end of input in unicode escape"));
            };
            value = (value << 4)
                | ch.to_digit(16)
                    .ok_or_else(|| JsonError::new("invalid unicode escape"))?;
        }
        char::from_u32(value).ok_or_else(|| JsonError::new("invalid unicode scalar value"))
    }

    fn parse_array(&mut self) -> Result<JsonValue, JsonError> {
        self.expect('[')?;
        let mut values = Vec::new();
        loop {
            self.skip_whitespace();
            if self.try_consume(']') {
                break;
            }
            values.push(self.parse_value()?);
            self.skip_whitespace();
            if self.try_consume(']') {
                break;
            }
            self.expect(',')?;
        }
        Ok(JsonValue::Array(values))
    }

    fn parse_object(&mut self) -> Result<JsonValue, JsonError> {
        self.expect('{')?;
        let mut entries = BTreeMap::new();
        loop {
            self.skip_whitespace();
            if self.try_consume('}') {
                break;
            }
            let key = self.parse_string()?;
            self.skip_whitespace();
            self.expect(':')?;
            let value = self.parse_value()?;
            entries.insert(key, value);
            self.skip_whitespace();
            if self.try_consume('}') {
                break;
            }
            self.expect(',')?;
        }
        Ok(JsonValue::Object(entries))
    }

    fn parse_number(&mut self) -> Result<i64, JsonError> {
        let mut value = String::new();
        if self.try_consume('-') {
            value.push('-');
        }

        while let Some(ch @ '0'..='9') = self.peek() {
            value.push(ch);
            self.index += 1;
        }

        if value.is_empty() || value == "-" {
            return Err(JsonError::new("invalid number"));
        }

        value
            .parse::<i64>()
            .map_err(|_| JsonError::new("number out of range"))
    }

    fn expect(&mut self, expected: char) -> Result<(), JsonError> {
        match self.next() {
            Some(actual) if actual == expected => Ok(()),
            Some(actual) => Err(JsonError::new(format!(
                "expected '{expected}', found '{actual}'"
            ))),
            None => Err(JsonError::new(format!(
                "expected '{expected}', found end of input"
            ))),
        }
    }

    fn try_consume(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.index += 1;
            true
        } else {
            false
        }
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.peek(), Some(' ' | '\n' | '\r' | '\t')) {
            self.index += 1;
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.index).copied()
    }

    fn next(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.index += 1;
        Some(ch)
    }

    fn is_eof(&self) -> bool {
        self.index >= self.chars.len()
    }
}

#[cfg(test)]
mod tests {
    use super::{render_string, JsonValue};
    use std::collections::BTreeMap;

    #[test]
    fn renders_and_parses_json_values() {
        let mut object = BTreeMap::new();
        object.insert("flag".to_string(), JsonValue::Bool(true));
        object.insert(
            "items".to_string(),
            JsonValue::Array(vec![
                JsonValue::Number(4),
                JsonValue::String("ok".to_string()),
            ]),
        );

        let rendered = JsonValue::Object(object).render();
        let parsed = JsonValue::parse(&rendered).expect("json should parse");

        assert_eq!(parsed.as_object().expect("object").len(), 2);
    }

    #[test]
    fn escapes_control_characters() {
        assert_eq!(render_string("a\n\t\"b"), "\"a\\n\\t\\\"b\"");
    }

    #[test]
    fn parses_escape_sequences_inside_strings() {
        // Short escapes must decode to the control character they name rather
        // than to the literal letter that follows the backslash.
        assert_eq!(
            JsonValue::parse("\"a\\nb\"").expect("newline escape parses"),
            JsonValue::String("a\nb".to_string())
        );
        assert_eq!(
            JsonValue::parse("\"a\\tb\"").expect("tab escape parses"),
            JsonValue::String("a\tb".to_string())
        );
        assert_eq!(
            JsonValue::parse("\"\\r\\b\\f\"").expect("control escapes parse"),
            JsonValue::String("\r\u{08}\u{0C}".to_string())
        );
        // JSON allows a forward slash to be escaped even though it need not be.
        assert_eq!(
            JsonValue::parse("\"a\\/b\"").expect("escaped slash parses"),
            JsonValue::String("a/b".to_string())
        );
        // An escaped quote must not terminate the string, and an escaped
        // backslash must not start another escape sequence.
        assert_eq!(
            JsonValue::parse("\"\\\"\\\\\"").expect("escaped quote and backslash parse"),
            JsonValue::String("\"\\".to_string())
        );
    }

    #[test]
    fn parses_four_digit_unicode_escapes() {
        // All four hex digits belong to the escape: stopping after three would
        // leave the fourth as a literal character.
        assert_eq!(
            JsonValue::parse("\"\\u0041\"").expect("four digit escape parses"),
            JsonValue::String("A".to_string())
        );
        // Hex digits are accepted in either case.
        assert_eq!(
            JsonValue::parse("\"\\u00E9\"").expect("uppercase hex digits parse"),
            JsonValue::String("\u{e9}".to_string())
        );
        assert_eq!(
            JsonValue::parse("\"\\u00e9\"").expect("lowercase hex digits parse"),
            JsonValue::String("\u{e9}".to_string())
        );
        // Fewer than four hex digits is not a unicode escape.
        assert!(JsonValue::parse("\"\\u00e\"").is_err(), "three hex digits");
        assert!(JsonValue::parse("\"\\uZZZZ\"").is_err(), "non hex digits");
        // A surrogate is not a unicode scalar value, so it cannot be a char.
        assert!(JsonValue::parse("\"\\ud800\"").is_err(), "lone surrogate");
    }

    #[test]
    fn round_trips_parsed_escapes_through_the_renderer() {
        // Every escape that parses must render back to JSON that re-parses to
        // the same value. The renderer may pick an equivalent spelling (for
        // example \u000a instead of \n), so only the round trip is asserted.
        for source in [
            "\"a\\nb\"",
            "\"a\\tb\"",
            "\"a\\/b\"",
            "\"\\r\\b\\f\"",
            "\"\\\"\\\\\"",
            "\"\\u0041\"",
            "\"\\u00E9\"",
        ] {
            let parsed = JsonValue::parse(source).expect("escaped source parses");
            let reparsed = JsonValue::parse(&parsed.render()).expect("rendered value re-parses");
            assert_eq!(reparsed, parsed, "round trip changed {source}");
        }

        // Object keys are escaped on the way out and unescaped on the way in.
        let mut object = BTreeMap::new();
        object.insert("a\nb".to_string(), JsonValue::String("c\td".to_string()));
        let original = JsonValue::Object(object);
        let reparsed = JsonValue::parse(&original.render()).expect("escaped key re-parses");
        assert_eq!(reparsed, original);
    }

    #[test]
    fn parses_the_i64_boundary_and_rejects_the_values_past_it() {
        // i64::MAX and i64::MIN are representable, so they must parse to the
        // exact value rather than being narrowed or clamped.
        assert_eq!(
            JsonValue::parse("9223372036854775807")
                .expect("i64::MAX parses")
                .as_i64(),
            Some(i64::MAX)
        );
        assert_eq!(
            JsonValue::parse("-9223372036854775808")
                .expect("i64::MIN parses")
                .as_i64(),
            Some(i64::MIN)
        );
        assert_eq!(
            JsonValue::parse("-9223372036854775807")
                .expect("i64::MIN + 1 parses")
                .as_i64(),
            Some(i64::MIN + 1)
        );

        // One past either end of the range must be rejected. This is what pins
        // the accepted range at i64: a rejection test built from an obviously
        // huge number is also satisfied by a much narrower integer type.
        assert!(
            JsonValue::parse("9223372036854775808").is_err(),
            "i64::MAX + 1"
        );
        assert!(
            JsonValue::parse("-9223372036854775809").is_err(),
            "i64::MIN - 1"
        );
    }
}
