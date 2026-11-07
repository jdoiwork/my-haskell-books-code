// Effects as requests, for the chapter "Effects".
// 頼みごととしてのエフェクト。「エフェクト」の章

// ANCHOR: effects
// Koka
effect dice
  fun roll(sides : int) : int

fun attack() : <dice,console> int
  val hit = roll(20)
  if hit >= 10 then
    val damage = roll(6)
    println("hit for " ++ damage.show)
    damage
  else
    println("missed")
    0

// A loaded die: always the top face.
// いかさまサイコロ：いつもいちばん大きい目
fun loaded(action)
  with fun roll(sides) sides
  action()
// ANCHOR_END: effects

// A die that always shows 1.
// いつも 1 が出るサイコロ
fun unlucky(action)
  with fun roll(sides) 1
  action()

fun main()
  if loaded(attack) != 6 then throw("loaded: wrong result")
  if unlucky(attack) != 0 then throw("unlucky: wrong result")
  println("ok")
