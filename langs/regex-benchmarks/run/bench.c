// The chapter on measuring (MiniRegex): the same jobs as MiniRegex's
// benchmark, with the C library of PCRE2 (Homebrew's), without and
// with the JIT. Built by check_benchmarks.py:
//   clang -O2 bench.c -lpcre2-8; ./a.out LINES ROUNDS
// Prints one line for each job: the engine, the job, the count, the
// checksum, and the middle time of the rounds in microseconds.
// 測って比べる章（MiniRegex）：MiniRegex のベンチマークと同じ仕事を、
// PCRE2 の C のライブラリ（Homebrew のもの）で、JIT なしと JIT ありで。
// 仕事ごとに1行：エンジン、仕事、件数、確かめの数、回のまん中の時間
// （マイクロ秒）
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

static const char *NAMES[] = {"Mira", "Ruby", "Lam", "GHC",
                              "Perl", "Rust", "Python", "Go"};
static const char *LOWER[] = {"mira", "ruby", "lam", "ghc",
                              "perl", "rust", "python", "go"};
static const char *ITEMS[] = {"herbs", "potions", "swords", "maps",
                              "scrolls"};

// 📒 The guild's book of sales: n lines, the same in every language.
// ギルドの売り上げの帳面：n 行。どの言語でも同じもの
static char *guild_book(int n, size_t *len) {
  size_t cap = (size_t)n * 120 + 1;
  char *text = malloc(cap);
  size_t at = 0;
  for (int i = 0; i < n; i++) {
    at += (size_t)snprintf(
        text + at, cap - at,
        "2026-11-%02d %02d:%02d %s sold %d %s to %s for %d gold",
        i % 28 + 1, i % 24, i * 7 % 60, NAMES[i % 8], i % 9 + 1,
        ITEMS[i % 5], NAMES[(3 * i + 1) % 8], i * 37 % 1000);
    if (i % 7 == 0)
      at += (size_t)snprintf(text + at, cap - at, " <%s@guild.example>",
                             LOWER[i % 8]);
    text[at++] = '\n';
  }
  text[at] = '\0';
  *len = at;
  return text;
}

static double now_us(void) {
  struct timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);
  return t.tv_sec * 1e6 + t.tv_nsec / 1e3;
}

static int by_value(const void *a, const void *b) {
  double x = *(const double *)a, y = *(const double *)b;
  return (x > y) - (x < y);
}

// ANCHOR: count
// 🔢 Count the matches, from left to right; the checksum adds up group
//    2 where there is one. The text is checked to be UTF-8 only at the
//    first match: from then on, PCRE2_NO_UTF_CHECK.
// 左から一致を数える。確かめの数は、2番のグループがあれば、その数の
// 合計。文字列が UTF-8 かを確かめるのは、最初の1回だけ。そのあとは
// PCRE2_NO_UTF_CHECK
static long count(pcre2_code *re, const char *text, size_t len,
                  long *total) {
  pcre2_match_data *md = pcre2_match_data_create_from_pattern(re, NULL);
  uint32_t groups;
  pcre2_pattern_info(re, PCRE2_INFO_CAPTURECOUNT, &groups);
  long n = 0;
  size_t from = 0;
  uint32_t check = 0;
  *total = 0;
  int rc;
  while ((rc = pcre2_match(re, (PCRE2_SPTR)text, len, from, check, md,
                           NULL)) >= 0) {
    PCRE2_SIZE *ov = pcre2_get_ovector_pointer(md);
    n++;
    if (groups >= 2) *total += atol(text + ov[4]);
    from = ov[1] > ov[0] ? ov[1] : ov[1] + 1;
    check = PCRE2_NO_UTF_CHECK;
  }
  pcre2_match_data_free(md);
  // no more matches, or an error (such as the match limit): -1
  // もう一致がないか、エラー（ステップ数の上限など）なら -1
  return rc == PCRE2_ERROR_NOMATCH ? n : -1;
}
// ANCHOR_END: count

static void run(const char *engine, int jit, const char *job,
                const char *pattern, const char *text, size_t len,
                int rounds) {
  int err;
  PCRE2_SIZE where;
  pcre2_code *re =
      pcre2_compile((PCRE2_SPTR)pattern, PCRE2_ZERO_TERMINATED,
                    PCRE2_UTF | PCRE2_UCP, &err, &where, NULL);
  if (re == NULL) exit(1);
  if (jit && pcre2_jit_compile(re, PCRE2_JIT_COMPLETE) != 0) exit(2);
  double *times = malloc(sizeof(double) * (size_t)rounds);
  long n = 0, total = 0;
  for (int r = 0; r < rounds; r++) {
    double start = now_us();
    n = count(re, text, len, &total);
    times[r] = now_us() - start;
  }
  qsort(times, (size_t)rounds, sizeof(double), by_value);
  printf("%s %s %ld %ld %.2f\n", engine, job, n, total, times[rounds / 2]);
  free(times);
  pcre2_code_free(re);
}

int main(int argc, char **argv) {
  if (argc < 3) return 1;
  int lines = atoi(argv[1]), rounds = atoi(argv[2]);
  size_t len;
  char *text = guild_book(lines, &len);
  const char *jobs[][2] = {
      {"dates", "\\d{4}-\\d{2}-\\d{2}"},
      {"mail", "[\\w.+-]+@[\\w-]+\\.[\\w.-]+"},
      {"names", "Mira|Ruby|Lam|GHC|Perl|Rust|Python|Go"},
      {"caseless", "(?i)perl"},
      {"sales", "(\\w+) sold (\\d+) (\\w+)"},
  };
  // the name field: one search, eight names and a ! by mistake
  // 名前の欄：探すのは1回。名前が8個と、まちがえて入った !
  const char *field = "Mira Ruby Lam GHC Perl Mira Ruby Lam!";
  for (int jit = 0; jit <= 1; jit++) {
    const char *engine = jit ? "pcre2-jit" : "pcre2";
    for (int j = 0; j < 5; j++)
      run(engine, jit, jobs[j][0], jobs[j][1], text, len, rounds);
    run(engine, jit, "field", "^(\\w+\\s?)+$", field, strlen(field),
        rounds);
  }
  free(text);
  return 0;
}
