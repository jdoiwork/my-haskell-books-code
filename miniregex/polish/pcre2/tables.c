// What the real PCRE2 says, to compare MiniRegex with.
// 本物の PCRE2 の答え。MiniRegex とくらべるため
//
//   tables props NAME...  : for each \p{NAME}, the ranges of the code
//                           points it matches ("NAME lo hi")
//   tables props の答え：\p{NAME} ごとに、合うコードポイントの範囲
//   tables clusters < in  : for each line, where \X ends when it starts
//                           at each character
//   tables clusters の答え：行ごとに、各文字から始めた \X の終わり
//
// Build it with the PCRE2 10.49 of Homebrew:
// Homebrew の PCRE2 10.49 でビルドする：
//   cc -O2 $(pcre2-config --cflags) pcre2/tables.c $(pcre2-config --libs8)
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// 🔤 One code point, in UTF-8. Gives back how many bytes.
// コードポイント1つを UTF-8 に。何バイトかを返す
static int utf8(unsigned c, unsigned char *out) {
  if (c < 0x80) { out[0] = c; return 1; }
  if (c < 0x800) { out[0] = 0xC0 | c >> 6; out[1] = 0x80 | (c & 0x3F); return 2; }
  if (c < 0x10000) {
    out[0] = 0xE0 | c >> 12; out[1] = 0x80 | (c >> 6 & 0x3F);
    out[2] = 0x80 | (c & 0x3F); return 3;
  }
  out[0] = 0xF0 | c >> 18; out[1] = 0x80 | (c >> 12 & 0x3F);
  out[2] = 0x80 | (c >> 6 & 0x3F); out[3] = 0x80 | (c & 0x3F); return 4;
}

static pcre2_code *compile(const char *pattern) {
  int error; PCRE2_SIZE offset;
  pcre2_code *re = pcre2_compile((PCRE2_SPTR)pattern, PCRE2_ZERO_TERMINATED,
                                 PCRE2_UTF, &error, &offset, NULL);
  if (re == NULL) { fprintf(stderr, "cannot compile %s\n", pattern); exit(2); }
  pcre2_jit_compile(re, PCRE2_JIT_COMPLETE);
  return re;
}

// ANCHOR: props
// 🏷️ props: every code point but the surrogates, one by one.
// props：サロゲートのほかの、全部のコードポイントを1つずつ
static void props(int n, char **names) {
  pcre2_match_data *md = pcre2_match_data_create(4, NULL);
  for (int i = 0; i < n; i++) {
    char pattern[200];
    snprintf(pattern, sizeof pattern, "^\\p{%s}", names[i]);
    pcre2_code *re = compile(pattern);
    long lo = -1;
    for (unsigned c = 0; c <= 0x110000; c++) {
      // (a surrogate cannot be in UTF-8: it ends a range)
      // （サロゲートは UTF-8 に書けないので、範囲はそこで切れる）
      int yes = 0;
      if (c <= 0x10FFFF && !(c >= 0xD800 && c <= 0xDFFF)) {
        unsigned char s[4]; int len = utf8(c, s);
        yes = pcre2_match(re, s, len, 0, 0, md, NULL) >= 0;
      }
      if (yes && lo < 0) lo = c;
      if (!yes && lo >= 0) {
        printf("%s %lX %X\n", names[i], lo, c - 1);
        lo = -1;
      }
    }
    pcre2_code_free(re);
  }
  pcre2_match_data_free(md);
}
// ANCHOR_END: props

// 🧩 clusters: for each line, start \X at each character, and tell
//    after how many characters it ends. The characters before the
//    start are matched by the pattern ((?s)^.{i}(\X)), not skipped
//    with an offset: then PCRE2 sees them, as a \X after .  does (it
//    counts the regional indicators just before, for GB12 and GB13).
// clusters：行ごとに、各文字から \X を始めて、何文字目で終わるかを言う。
// 始まりの前の文字は、offset で飛ばさず、パターン（(?s)^.{i}(\X)）で
// 読む。そうすると PCRE2 は、. のあとの \X と同じく、手前の文字を見る
// （GB12・GB13 のために、すぐ手前の地域指示子を数える）
static void clusters(void) {
  pcre2_match_data *md = pcre2_match_data_create(4, NULL);
  char line[4096];
  while (fgets(line, sizeof line, stdin) != NULL) {
    size_t len = strcspn(line, "\n");
    line[len] = '\0';
    // where each character starts, in bytes
    // 各文字が何バイト目から始まるか
    size_t starts[1024]; int count = 0;
    for (size_t b = 0; b < len; b++)
      if (((unsigned char)line[b] & 0xC0) != 0x80) starts[count++] = b;
    for (int i = 0; i < count; i++) {
      char pattern[64];
      snprintf(pattern, sizeof pattern, "(?s)^.{%d}(\\X)", i);
      pcre2_code *re = compile(pattern);
      int rc = pcre2_match(re, (PCRE2_SPTR)line, len, 0, 0, md, NULL);
      PCRE2_SIZE end = rc >= 0 ? pcre2_get_ovector_pointer(md)[3] : 0;
      pcre2_code_free(re);
      int k = i;
      while (k < count && starts[k] < end) k++;
      printf(i == 0 ? "%d" : " %d", k);
    }
    printf("\n");
  }
  pcre2_match_data_free(md);
}

int main(int argc, char **argv) {
  if (argc >= 2 && strcmp(argv[1], "props") == 0) props(argc - 2, argv + 2);
  else if (argc == 2 && strcmp(argv[1], "clusters") == 0) clusters();
  else { fprintf(stderr, "usage: tables props NAME... | clusters\n"); return 2; }
  return 0;
}
