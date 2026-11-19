// RE2 (C++): the name field, a back reference, and the two ways to
// choose among matches. Run by check_re2.py in Docker.
// RE2（C++）：名前の欄、後方参照、一致の選び方の2通り。check_re2.py が
// Docker の中で動かす
#include <re2/re2.h>

#include <cassert>
#include <chrono>
#include <iostream>
#include <string>

// 🪪 A name field with n names, and a "!" by mistake.
// 名前が n 個と、まちがえて入った「!」の名前の欄
std::string field_of(int n) {
  const char *names[] = {"Mira", "Ruby", "Lam", "GHC", "Perl"};
  std::string field;
  for (int i = 0; i < n; i++) {
    if (i > 0) field += " ";
    field += names[i % 5];
  }
  return field + "!";
}

int main() {
  auto start = std::chrono::steady_clock::now();
  // ANCHOR: field
  // C++ (RE2): the name field with 10000 names
  // C++（RE2）：名前が 10000 個の名前の欄
  RE2 names("^(\\w+\\s?)+$");
  std::string field = field_of(10000);
  std::cout << RE2::PartialMatch(field, names) << "\n";
  // ANCHOR_END: field
  double seconds = std::chrono::duration<double>(
      std::chrono::steady_clock::now() - start).count();
  assert(!RE2::PartialMatch(field, names));
  assert(seconds < 1.0);

  // ANCHOR: refuse
  // C++ (RE2): a back reference is an error when the RE2 is made
  // C++（RE2）：後方参照は、RE2 を作るときにエラー
  RE2::Options quiet;
  quiet.set_log_errors(false);
  RE2 twice("(\\w+) \\1", quiet);
  std::cout << twice.ok() << " " << twice.error() << "\n";
  // ANCHOR_END: refuse
  assert(!twice.ok());

  // ANCHOR: longest
  // C++ (RE2): a|ab finds "a", as Perl does; with longest_match, "ab"
  // C++（RE2）：a|ab は Perl と同じく「a」。longest_match なら「ab」
  std::string found;
  RE2 first("(a|ab)");
  RE2::PartialMatch("ab", first, &found);
  std::cout << found << "\n";
  RE2::Options longest;
  longest.set_longest_match(true);
  RE2 posix("(a|ab)", longest);
  RE2::PartialMatch("ab", posix, &found);
  std::cout << found << "\n";
  // ANCHOR_END: longest
  assert(found == "ab");
  return 0;
}
