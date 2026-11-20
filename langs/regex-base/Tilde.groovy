// The Groovy code in the regex-base chapter: =~ gives a Matcher, and
// ==~ asks whether the whole text matches. check_tilde.py runs it with
// Groovy 6 in Docker.
// regex-base の章の Groovy のコード：=~ は Matcher を返し、==~ は全体が
// 合うかを聞く。check_tilde.py が Docker の Groovy 6 で動かす
def line = "2026-11-21 Mira sold 300 gold, Ruby sold 25 gold"

// ANCHOR: groovy
// Groovy: =~ makes a Matcher; it is true in if when it finds something
// Groovy：=~ は Matcher を作る。if の中では、見つかれば真
def m = line =~ /(\w+) sold (\d+)/
println "type: ${m.getClass().simpleName}"
if (m) {
    println "found: ${m[0][1]} ${m[0][2]}"
}
println "all: ${(line =~ /(\d+) gold/).collect { it[1] }}"
// ==~ : does the whole text match? (true or false)
// ==~ ：文字列の全体が合うか（真か偽）
println "whole: ${line ==~ /\d{4}-.*/} ${line ==~ /gold/}"
// ANCHOR_END: groovy
