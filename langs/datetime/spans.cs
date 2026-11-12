// The C# code in the "dates and times" chapter.
// 日付と時刻の章の C# のコード
using System.Globalization;

var inv = CultureInfo.InvariantCulture;

// ANCHOR: timespan
// C# (a TimeSpan: a length of time; "hh" is the hours of the day only)
// C#（TimeSpan：時間の長さ。"hh" は1日の中の時間だけ）
var left = new TimeSpan(26, 5, 7);                 // 1.02:05:07
var careless = left.ToString(@"hh\:mm\:ss", inv);   // "02:05:07"
var shown = $"{(int)left.TotalHours}:{left:mm\\:ss}";  // "26:05:07"
// ANCHOR_END: timespan

Check(left.ToString() == "1.02:05:07");
Check(careless == "02:05:07");
Check(shown == "26:05:07");

// ANCHOR: months
// C# (AddMonths clips a missing day to the month's end)
// C#（AddMonths は、ない日を月末に切り詰める）
var first = new DateOnly(2027, 1, 31);
var payDays = Enumerable.Range(0, 4)
    .Select(n => first.AddMonths(n))
    .ToList();                    // 01-31, 02-28, 03-31, 04-30
// ANCHOR_END: months

Check(payDays.Select(d => d.ToString("MM-dd", inv))
             .SequenceEqual(["01-31", "02-28", "03-31", "04-30"]));
Console.WriteLine("ok");

static void Check(bool ok)
{
    if (!ok) throw new Exception("wrong result");
}
