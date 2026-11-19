# The Python code in the parser chapter of MiniRegex: the tree that the
# re module makes, how it tells a wrong pattern, and a { that is not a
# quantifier.
# MiniRegex のパーサーの章の Python のコード：re モジュールが作る木、
# まちがったパターンの知らせ方、量指定子でない {
import contextlib
import io
import re

shown = io.StringIO()
with contextlib.redirect_stdout(shown):
    # ANCHOR: tree
    # Python: re.DEBUG prints the tree that the parser made
    # Python：re.DEBUG で、パーサーが作った木を見せてくれる
    re.compile(r"colou?r|gr(a|e)y", re.DEBUG)
    # ANCHOR_END: tree
tree = shown.getvalue()
print(tree.split("\n\n")[0])
assert "MAX_REPEAT 0 1" in tree and "SUBPATTERN 1" in tree


# ANCHOR: errors
# Python: re.error knows the message and the place (pos)
# Python：re.error は、文と場所（pos）を持っている
def tell(pattern):
    try:
        re.compile(pattern)
        return "ok"
    except re.error as e:
        return f"{e.msg} (pos {e.pos})"


for p in ["a{5,2}", "[z-a]", "(ab", "ab)", "a**"]:
    print(f"{p:8} {tell(p)}")
# ANCHOR_END: errors

assert tell("a{5,2}") == "min repeat greater than max repeat (pos 2)"
assert tell("[z-a]") == "bad character range z-a (pos 1)"
assert tell("(ab") == "missing ), unterminated subpattern (pos 0)"
assert tell("ab)") == "unbalanced parenthesis (pos 2)"
assert tell("a**") == "multiple repeat (pos 2)"

# ANCHOR: braces
# Python: x{a} is the characters; a{,3} is 0 to 3 times
# Python：x{a} はただの文字。a{,3} は 0 回から 3 回
print(re.fullmatch(r"x{a}", "x{a}") is not None)
print(re.fullmatch(r"a{,3}", "aaa") is not None)
print(re.fullmatch(r"a{ 2 , 3 }", "aa") is not None)
# ANCHOR_END: braces

assert re.fullmatch(r"x{a}", "x{a}")
assert re.fullmatch(r"a{,3}", "aaa")
# Python reads a{ 2 , 3 } as the characters (PCRE2 as a quantifier)
# Python は a{ 2 , 3 } を、ただの文字と読む（PCRE2 は量指定子）
assert re.fullmatch(r"a{ 2 , 3 }", "aa") is None
assert re.fullmatch(r"a{ 2 , 3 }", "a{ 2 , 3 }")
