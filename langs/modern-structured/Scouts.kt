// Scouts sent out at the same time, for the chapter
// "Structured concurrency".
// 同時に送り出す偵察隊。「構造化された並行性」の章
import kotlinx.coroutines.*
import java.util.Collections

val report: MutableList<String> = Collections.synchronizedList(mutableListOf())

suspend fun scout(name: String, millis: Long): String {
    delay(millis)
    report.add(name)
    return name
}

suspend fun trap(millis: Long): String {
    delay(millis)
    throw IllegalStateException("fell into a trap")
}

// ANCHOR: scope
// Kotlin (coroutineScope waits for every child)
// Kotlin（coroutineScope は、子どもを全部待つ）
suspend fun party(): List<String> = coroutineScope {
    val slime  = async { scout("slime", 50) }
    val trapped = async { trap(10) }
    val goblin = async { scout("goblin", 100) }
    listOf(slime.await(), trapped.await(), goblin.await())
}
// ANCHOR_END: scope

fun main() = runBlocking {
    val failed = runCatching { party() }.exceptionOrNull()
    check(failed is IllegalStateException)
    // The others were cancelled: nobody comes back later.
    // ほかの子どもはキャンセルされた。あとから帰ってくる者はいない
    delay(200)
    check(report.isEmpty())
    println("ok")
}
