// The outcome of a quest, for the chapter "ADTs are everywhere".
// A compact source file (Java 25): run it with `java Outcome.java`.
// 依頼の結果。「代数的データ型は、どこにでもある」の章。
// Java 25 の1ファイルのプログラム。`java Outcome.java` で動く

// ANCHOR: sealed
// Java 21 (sealed interface, records and switch)
// Java 21（sealed なインターフェース、record、switch）
sealed interface Outcome permits Cleared, Failed, Fled {}
record Cleared(int gold) implements Outcome {}
record Failed(String place) implements Outcome {}
record Fled() implements Outcome {}

static String describe(Outcome outcome) {
    return switch (outcome) {
        case Cleared(int gold)     -> "cleared! +" + gold + " G";
        case Failed(String place)  -> "failed at " + place;
        case Fled()                -> "ran away";
    };
}
// ANCHOR_END: sealed

void check(boolean ok) {
    if (!ok) throw new AssertionError("wrong result");
}

void main() {
    check(describe(new Cleared(50)).equals("cleared! +50 G"));
    check(describe(new Failed("cave")).equals("failed at cave"));
    check(describe(new Fled()).equals("ran away"));
    IO.println("ok");
}
