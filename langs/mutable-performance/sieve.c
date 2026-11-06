// The C code in the "rewrite to go faster" chapter.
// 書き換えて速くするの章の C のコード
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// ANCHOR: array
// C (a block of bools from malloc, rewritten in place)
// C（malloc でもらった bool の並びを、その場で書き換える）
bool *sieve(int n) {
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
    return is_prime;  // the caller must free it / 呼んだ人が free する
}
// ANCHOR_END: array

static int count_primes(int n) {
    bool *is_prime = sieve(n);
    int count = 0;
    for (int i = 0; i <= n; i++) count += is_prime[i];
    free(is_prime);
    return count;
}

int main(void) {
    bool *small = sieve(30);
    int expected[] = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29};
    int k = 0;
    for (int i = 0; i <= 30; i++) {
        if (small[i]) assert(i == expected[k++]);
    }
    assert(k == 10);
    free(small);
    assert(count_primes(1000000) == 78498);
    puts("ok");
    return 0;
}
