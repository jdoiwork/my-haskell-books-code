// The Rust code in the "dates and times" chapter (jiff's Span).
// 日付と時刻の章の Rust のコード（jiff の Span）
use jiff::{ToSpan, Unit, civil::date};

#[test]
fn spans_of_the_guild() -> Result<(), jiff::Error> {
    // ANCHOR: span
    // Rust (jiff: one Span can hold months, days, and hours)
    // Rust（jiff：1つの Span に、月も日も時間も持てる）
    let term = 1.month().days(10);
    let ends = date(2027, 1, 31).checked_add(term)?;        // 2027-03-10
    let served = date(2027, 1, 31)
        .until((Unit::Month, date(2027, 3, 15)))?;           // P1M15D
    let left = 26.hours().minutes(5).seconds(7);
    let hours = left.total(Unit::Hour)?;                     // 26.085...
    // ANCHOR_END: span
    assert_eq!(ends, date(2027, 3, 10));
    assert_eq!(served.to_string(), "P1M15D");
    assert_eq!(hours as i64, 26);
    assert_eq!(date(2027, 1, 31).checked_add(1.month())?, date(2027, 2, 28));
    Ok(())
}
