// The outcome of a quest, for the chapter "ADTs are everywhere".
// 依頼の結果。「代数的データ型は、どこにでもある」の章

// The checks come first: C# wants top-level statements before the types.
// 確かめを先に書く（C# では、トップレベルの文を型の宣言より前に書く）
Check(Describe(new Cleared(50)) == "cleared! +50 G");
Check(Describe(new Failed("cave")) == "failed at cave");
Check(Describe(new Fled()) == "ran away");
Console.WriteLine("ok");

static void Check(bool ok)
{
    if (!ok) throw new Exception("wrong result");
}

// ANCHOR: describe
static string Describe(Outcome outcome) => outcome switch
{
    Cleared c => $"cleared! +{c.Gold} G",
    Failed f  => $"failed at {f.Place}",
    Fled      => "ran away",
    _         => throw new ArgumentOutOfRangeException(nameof(outcome)),
};
// ANCHOR_END: describe

// ANCHOR: records
// C# (records and a switch expression)
// C#（record と switch 式）
abstract record Outcome;
record Cleared(int Gold) : Outcome;
record Failed(string Place) : Outcome;
record Fled : Outcome;
// ANCHOR_END: records
