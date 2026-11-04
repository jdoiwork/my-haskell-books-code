// The Rust code in the security chapter.
// セキュリティの章の Rust のコード

// ANCHOR: newtype
// Rust (a newtype with a private field, and a secret that hides itself)
// Rust（フィールドを隠した newtype と、自分を隠す秘密）
mod auth {
    use std::fmt;

    #[derive(Debug, PartialEq)]
    pub struct Username(String); // the field is private / フィールドは非公開

    impl Username {
        pub fn parse(raw: &str) -> Result<Username, String> {
            let ok = (3..=16).contains(&raw.len())
                && raw.chars().all(|c| {
                    c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_'
                });
            if ok {
                Ok(Username(raw.to_string()))
            } else {
                Err(format!("bad username: {raw}"))
            }
        }
    }

    pub struct Password(String);

    impl Password {
        pub fn new(raw: &str) -> Password {
            Password(raw.to_string())
        }
        pub fn reveal(&self) -> &str {
            &self.0
        }
    }

    impl fmt::Debug for Password {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "<redacted>")
        }
    }
}
// ANCHOR_END: newtype

use auth::{Password, Username};

fn main() {
    assert!(Username::parse("lambda_3").is_ok());
    assert!(Username::parse("x' OR '1'='1").is_err());
    let pass = Password::new("hunter2");
    assert_eq!(format!("{pass:?}"), "<redacted>");
    assert_eq!(pass.reveal(), "hunter2");
}
