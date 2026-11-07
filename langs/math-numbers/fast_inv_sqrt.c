// The fast inverse square root, for the chapter "What numbers really are".
// 高速な逆平方根。「数の正体」の章
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// ANCHOR: memcpy
// C（いまの正しい書き方。uint32_t には <stdint.h>、memcpy には <string.h>）
float fast_inv_sqrt(float x) {
    uint32_t bits;
    memcpy(&bits, &x, sizeof bits);
    bits = 0x5f3759df - (bits >> 1);
    float guess;
    memcpy(&guess, &bits, sizeof guess);
    return guess * (1.5f - 0.5f * x * guess * guess);
}
// ANCHOR_END: memcpy

int main(void) {
    // Within 0.2% of 1 / sqrt x, like the Haskell test.
    // Haskell のテストと同じく、本当の 1 / sqrt x と 0.2% もちがわない
    const float xs[] = {0.001f, 0.5f, 2.0f, 10.0f, 12345.0f, 1.0e6f};
    for (size_t i = 0; i < sizeof xs / sizeof xs[0]; i++) {
        if (fabsf(fast_inv_sqrt(xs[i]) * sqrtf(xs[i]) - 1.0f) >= 0.002f) {
            printf("too far at %g\n", xs[i]);
            return EXIT_FAILURE;
        }
    }
    printf("ok\n");
    return 0;
}
