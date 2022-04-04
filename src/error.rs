#[derive(Debug, PartialEq)]
pub struct Error {
    msg: String,
    offset: usize,
}

impl Error {
    pub fn new(msg: &str, offset: usize) -> Self {
        Self {
            msg: msg.to_string(),
            offset,
        }
    }

    pub fn format(&self, src: &str) -> String {
        let (row, column) = self.calc_row_column(src);

        let line = src.split("\n").nth(row - 1).unwrap_or("");

        let row = match row {
            0..=9 => format!("    {}", row),
            10..=99 => format!("   {}", row),
            100..=999 => format!("  {}", row),
            _ => format!(" {}", row),
        };

        let spaces = (0..column + row.len() - 1)
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

    fn calc_row_column(&self, src: &str) -> (usize, usize) {
        let mut row = 1;
        let mut column = 0;

        for (i, c) in src.chars().enumerate() {
            if c == '\n' {
                row += 1;
                column = 0;
            } else {
                column += 1;
            }

            if i == self.offset {
                break;
            }
        }

        (row, column)
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

        let error = Error::new("Unknown field", 74);

        assert_eq!(&error.format(&src), "Error: Unknown field

    6 |         this.lol = omg; // just a comment
                     ^--- Here.");
    }
}
