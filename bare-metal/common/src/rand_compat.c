#include <stdint.h>

/* Minimal rand/srand implementation to avoid pulling newlib's rand which
 * references newlib reentrancy data. Uses a simple LCG (same constants as
 * ANSI C implementations) and returns values in the range [0, RAND_MAX].
 */
static uint32_t rand_state = 1;

void srand(unsigned seed)
{
    rand_state = (uint32_t)seed;
}

int rand(void)
{
    rand_state = rand_state * 1103515245u + 12345u;
    return (int)(rand_state & 0x7fffffff);
}
