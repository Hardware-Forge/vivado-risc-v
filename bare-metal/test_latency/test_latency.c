#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

static inline uint64_t rdcycle(void) {
    uint64_t cycles;
    asm volatile ("rdcycle %0" : "=r"(cycles));
    return cycles;
}

int main()
{
    uint64_t start = rdcycle();
    volatile uint64_t x = *((volatile uint64_t*)0x80000100); // force cache miss
    uint64_t end = rdcycle();
    printf("Memory latency: %lu cycles\n", end - start);
}
