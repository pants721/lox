pub struct CamelCaseSplit<'a> {
    char_indices: std::str::CharIndices<'a>,
    chunk_start: usize,
    s: &'a str
}

impl<'a> CamelCaseSplit<'a> {
    pub fn new(s: &'a str) -> Self {
        let mut char_indices = s.char_indices();
        // We'll never want to split before the first char, so skip it.
        char_indices.next();
        Self {
            char_indices,
            chunk_start: 0,
            s,
        }
    }
}

impl<'a> Iterator for CamelCaseSplit<'a> {
    type Item = &'a str;
    
    fn next(&mut self) -> Option<Self::Item> {
        // The input is exhausted
        if self.chunk_start == self.s.len() {
            return None;
        }
        // Find the next uppercase letter position OR the end of the string
        let chunk_end = if let Some((chunk_end, _)) = self.char_indices.by_ref().find(|(_, c)| c.is_uppercase()) {
            chunk_end
        } else {
            self.s.len()
        };
        let chunk = &self.s[self.chunk_start..chunk_end];
        self.chunk_start = chunk_end;
        Some(chunk)
    }
}

#[macro_export]
/// Takes arguments (line_number, message, ...)
/// Message can be a format string.
/// Returns an error string in the format "[line $line] Error: $msg".
macro_rules! lox_error_str {
    ($line:expr, $msg:expr) => {
        format!("[line {}] Error: {}", $line, $msg)
    };

    ($line:expr, $msg:expr, $($arg:tt)*) => {
        format!("[line {}] Error: {}", $line, format_args!($msg, $($arg)*))
    };
}

#[macro_export]
macro_rules! lox_token_error_str {
    ($line:expr, $token:expr, $msg:expr) => {
        format!("[line {}] Error at '{}': {}", $line, $token, $msg)
    };

    ($line:expr, $msg:expr, $($arg:tt)*) => {
        format!("[line {}] Error at '{}': {}", $line, $token, format_args!($msg, $($arg)*))
    };
}
