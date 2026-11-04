// The Rust code in the error handling chapter.
// エラー処理の章の Rust のコード
#[derive(Debug, PartialEq)]
enum RegistrationError {
    EmptyName,
    TooYoung(i64),
    NotANumber,
}

#[derive(Debug, PartialEq)]
struct Adventurer {
    name: String,
    age: i64,
    job: String,
}

fn validate_name(name: &str) -> Result<String, RegistrationError> {
    if name.is_empty() { Err(RegistrationError::EmptyName) } else { Ok(name.to_string()) }
}

fn validate_age(age: &str) -> Result<i64, RegistrationError> {
    let a: i64 = age.parse().map_err(|_| RegistrationError::NotANumber)?;
    if a < 15 { Err(RegistrationError::TooYoung(a)) } else { Ok(a) }
}

fn validate_job(job: &str) -> Result<String, RegistrationError> {
    Ok(job.to_string())
}
// ANCHOR: code
// Rust
fn register_parts(name: &str, age: &str, job: &str)
    -> Result<Adventurer, RegistrationError> {
    let n = validate_name(name)?;
    let a = validate_age(age)?;
    let j = validate_job(job)?;
    Ok(Adventurer { name: n, age: a, job: j })
}
// ANCHOR_END: code
fn main() {
assert_eq!(register_parts("Rust", "13", "Warrior"), Err(RegistrationError::TooYoung(13)));
assert_eq!(register_parts("", "13", "Warrior"), Err(RegistrationError::EmptyName));
assert_eq!(register_parts("Rust", "x", "Warrior"), Err(RegistrationError::NotANumber));
assert!(register_parts("Rust", "16", "Warrior").is_ok());
}
