# Run vm.pl with Homebrew's Perl 5.44 (on the Mac, like the chapter),
# and check what it prints. Without Perl 5.44, it is skipped with a
# message.
# vm.pl を Homebrew の Perl 5.44（章と同じく Mac の上）で動かし、出力を
# 確かめる。Perl 5.44 がなければ、知らせて飛ばす
import os
import subprocess

here = os.path.dirname(os.path.abspath(__file__))
perl = "/opt/homebrew/bin/perl"
want = 'Compiling REx "colou?r|gr(a|e)y"\nFinal program:\n   1: TRIE-EXACT[cg] (buf:0/1) (31)\n      <colo> (5)\n   5:   CURLY{0,1} (11)\n   9:     EXACT <u> (0)\n  11:   EXACT <r> (31)\n      <gr> (17)\n  17:   OPEN1 (19)\n  19:     TRIE-EXACT[ae] (buf:1/1) (27)\n          <a> \n          <e> \n  27:   CLOSE1 (29)\n  29:   EXACT <y> (31)\n  31: END (0)\nstclass AHOCORASICK-EXACT[cg] (buf:0/1) minlen 3 \nnot found, 0.00s\nFreeing REx: "colou?r|gr(a|e)y"\n'

version = subprocess.run([perl, "-e", "print $^V"], capture_output=True,
                         text=True).stdout if os.path.exists(perl) else ""
if not version.startswith("v5.44"):
    print("skip: no Perl 5.44 (brew install perl)")
else:
    out = subprocess.run([perl, os.path.join(here, "vm.pl")],
                         capture_output=True, text=True, check=True).stdout
    assert out == want, out
    print(out, end="")
