// The outcome of a quest, for the chapter "ADTs are everywhere".
// 依頼の結果。「代数的データ型は、どこにでもある」の章

// ANCHOR: sealed
// Kotlin (sealed interface and when)
// Kotlin（sealed なインターフェースと when）
sealed interface Outcome
data class Cleared(val gold: Int) : Outcome
data class Failed(val place: String) : Outcome
data object Fled : Outcome

fun describe(outcome: Outcome): String = when (outcome) {
    is Cleared -> "cleared! +${outcome.gold} G"
    is Failed  -> "failed at ${outcome.place}"
    Fled       -> "ran away"
}
// ANCHOR_END: sealed

fun main() {
    check(describe(Cleared(50)) == "cleared! +50 G")
    check(describe(Failed("cave")) == "failed at cave")
    check(describe(Fled) == "ran away")
    println("ok")
}
