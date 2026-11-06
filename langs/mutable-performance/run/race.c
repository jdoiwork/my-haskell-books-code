// The sieve race: how long does a sieve up to n take? (the best of 3)
// ふるいの競走：n までのふるいに、どれだけかかる？（3回のうちの最速）
//   clang -O2 -o race race.c && ./race 10000000
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

static int sieve(int n) {
    bool *is_prime = malloc(n + 1);
    memset(is_prime, true, n + 1);
    is_prime[0] = false;
    is_prime[1] = false;
    for (int p = 2; p * p <= n; p++) {
        if (is_prime[p]) {
            for (int m = p * p; m <= n; m += p) {
                is_prime[m] = false;
            }
        }
    }
    int count = 0;
    for (int i = 0; i <= n; i++) count += is_prime[i];
    free(is_prime);
    return count;
}

static double now_ms(void) {
    struct timespec t;
    clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec * 1000.0 + t.tv_nsec / 1e6;
}

int main(int argc, char **argv) {
    if (argc < 2) return 1;
    int n = atoi(argv[1]);
    double best = 1e18;
    int count = 0;
    for (int i = 0; i < 3; i++) {
        double start = now_ms();
        count = sieve(n);
        double ms = now_ms() - start;
        if (ms < best) best = ms;
    }
    printf("%d %.1f\n", count, best);
    return 0;
}
