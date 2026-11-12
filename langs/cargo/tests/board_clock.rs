// The Rust code in the "time zones and summer time" chapter (jiff).
// タイムゾーンと夏時間の章の Rust のコード（jiff）
use jiff::{Timestamp, ToSpan, civil::date, tz::TimeZone};

#[test]
fn the_day_on_the_board() -> Result<(), jiff::Error> {
    // ANCHOR: jiff
    // Rust (jiff: a moment, a wall clock with its zone, and a day)
    // Rust（jiff：瞬間、時間帯つきの壁の時計、日付）
    let posted: Timestamp = "2026-11-12T21:00:00Z".parse()?;
    let tokyo = posted.in_tz("Asia/Tokyo")?;    // Zoned
    let today = tokyo.date();                   // civil::Date
    let due = today.checked_add(7.days())?;     // 2026-11-20
    // ANCHOR_END: jiff
    assert_eq!(tokyo.to_string(), "2026-11-13T06:00:00+09:00[Asia/Tokyo]");
    assert_eq!(today, date(2026, 11, 13));
    assert_eq!(due, date(2026, 11, 20));
    Ok(())
}

#[test]
fn a_skipped_time_in_new_york() -> Result<(), jiff::Error> {
    // ANCHOR: gap
    // Rust (jiff: asking for 2:30 on the morning the clock skips it)
    // Rust（jiff：時計が飛ばす朝の 2:30 を聞く）
    let new_york = TimeZone::get("America/New_York")?;
    let wall = date(2026, 3, 8).at(2, 30, 0, 0);
    let ambiguous = new_york.to_ambiguous_zoned(wall);
    let refused = ambiguous.clone().unambiguous();   // Err: it was skipped
    let later = ambiguous.compatible()?;             // 03:30 EDT
    // ANCHOR_END: gap
    assert!(refused.is_err());
    assert_eq!(later.to_string(), "2026-03-08T03:30:00-04:00[America/New_York]");
    Ok(())
}
