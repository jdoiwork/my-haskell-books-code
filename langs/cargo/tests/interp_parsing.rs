// The Rust code in the chapter on parsers: the nom crate, parser
// combinators for Rust.
// パーサーの章の Rust のコード：Rust のパーサーの組み合わせのクレート nom
use nom::{
    IResult, Parser,
    branch::alt,
    character::complete::{char, digit1, multispace0},
    combinator::{all_consuming, map_res},
    multi::many0,
    sequence::{delimited, pair},
};

// ANCHOR: calc
// Rust (nom): each parser is a function from the input to the rest
// and a value. expr and parens call each other, as functions do.
// Rust（nom）：パーサーは、入力から「残りと値」への関数。expr と
// parens は、関数どうしなので、互いを呼べる
type Error<'a> = nom::error::Error<&'a str>;

fn symbol<'a>(
    c: char,
) -> impl Parser<&'a str, Output = char, Error = Error<'a>> {
    delimited(multispace0, char(c), multispace0)
}

fn number(i: &str) -> IResult<&str, i64> {
    map_res(delimited(multispace0, digit1, multispace0), str::parse)
        .parse(i)
}

fn parens(i: &str) -> IResult<&str, i64> {
    delimited(symbol('('), expr, symbol(')')).parse(i)
}

fn factor(i: &str) -> IResult<&str, i64> {
    alt((number, parens)).parse(i)
}

fn term(i: &str) -> IResult<&str, i64> {
    let (i, first) = factor(i)?;
    let (i, rest) =
        many0(pair(alt((symbol('*'), symbol('/'))), factor)).parse(i)?;
    let value = rest.into_iter().fold(first, |acc, (op, x)| {
        if op == '*' { acc * x } else { acc.div_euclid(x) }
    });
    Ok((i, value))
}

fn expr(i: &str) -> IResult<&str, i64> {
    let (i, first) = term(i)?;
    let (i, rest) =
        many0(pair(alt((symbol('+'), symbol('-'))), term)).parse(i)?;
    let value = rest.into_iter().fold(first, |acc, (op, x)| {
        if op == '+' { acc + x } else { acc - x }
    });
    Ok((i, value))
}
// ANCHOR_END: calc

fn calc(text: &str) -> Option<i64> {
    all_consuming(expr).parse(text).ok().map(|(_, v)| v)
}

#[test]
fn computes_with_parentheses() {
    assert_eq!(calc("1 + 2 * 3"), Some(7));
    assert_eq!(calc("(1 + 2) * 3"), Some(9));
    assert_eq!(calc(" (1 + (2 * 3)) * 4 "), Some(28));
    assert_eq!(calc("10 - 4 - 3"), Some(3));
}

// ANCHOR: error
#[test]
fn tells_where_it_stopped() {
    // The rest of the input where it stopped, and the kind of the parser.
    // 止まった所から先の入力と、パーサーの種類
    let err = all_consuming(expr).parse("(1 + 2").unwrap_err();
    assert_eq!(
        format!("{err:?}"),
        r#"Error(Error { input: "", code: Char })"#
    );
}
// ANCHOR_END: error
