use regex::{self, Regex};
use std::sync::OnceLock;

pub fn white_space() -> &'static Regex {
    static REGES: OnceLock<Regex> = OnceLock::new();
    REGES.get_or_init(|| Regex::new(r"[ \t\x08\n]").unwrap())
}

pub fn number() -> &'static Regex {
    static REGEX: OnceLock<Regex> = OnceLock::new();
    REGEX.get_or_init(|| Regex::new(r"[0-9]").unwrap())
}

pub fn alphabet_underscore() -> &'static Regex {
    static REGEX: OnceLock<Regex> = OnceLock::new();
    REGEX.get_or_init(|| Regex::new(r"[_a-zA-Z]").unwrap())
}

pub fn alphabet_underscore_number() -> &'static Regex {
    static REGEX: OnceLock<Regex> = OnceLock::new();
    REGEX.get_or_init(|| Regex::new(r"[_a-zA-Z0-9]").unwrap())
}
