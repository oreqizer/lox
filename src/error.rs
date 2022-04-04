pub struct Error {
    msg: String,
    row: usize,
    column: usize,
}

impl Error {
    pub fn new(msg: &str, row: usize, column: usize) -> Self {
        Self {
            msg: msg.to_string(),
            row,
            column,
        }
    }

    pub fn format(&self, src: &str) -> String {
        let line = src.split("\n").nth(self.row - 1).unwrap_or("");

        let row = match self.row {
            0..=9 => format!("    {}", self.row),
            10..=99 => format!("   {}", self.row),
            100..=999 => format!("  {}", self.row),
            _ => format!(" {}", self.row),
        };

        let spaces = (0..self.column + row.len() - 1)
            .into_iter()
            .map(|_| ' ')
            .collect::<String>();

        format!(
            "Error: {msg:}\n\n{row:} | {line:}\n{spaces:}   ^--- Here.",
            msg = self.msg,
            row = row,
            line = line,
            spaces = spaces
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn format_error() {
        let src = r#"class Kek {
    kek = "bur";
    omg = 1337;

    Kek(omg) {
        this.lol = omg; // just a comment
    }

    fun lol() {
        print "bur";
    }
}
"#;

        let error = Error::new("Unknown identifier", 6, 14);

        assert_eq!(&error.format(&src), "Error: Unknown identifier

    6 |         this.lol = omg; // just a comment
                     ^--- Here.");
    }
}
