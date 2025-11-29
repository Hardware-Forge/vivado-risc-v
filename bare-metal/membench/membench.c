/*
 * RISC-V Memory Benchmark
 *
 * A collection of simple memory microbenchmarks implemented in C with
 * inline assembly to measure various memory access patterns and behaviors.
 *
 * This version is designed to be simulation-friendly by avoiding complex
 * dependencies and using simple timing via the rdcycle instruction.
 *
 * Each benchmark measures cycles taken for a specific memory access pattern
 * and prints results in a unified format.
 */
//#define ENABLE_VERIFY
//#define BENCH_FORCE_HIER
//#define BENCH_COALESCE_WRITE_SB
//#define BENCH_STRIDED_PREFETCH
//#define ENABLE_VERIFY
#include <stdint.h>
#include <stdio.h>
#include <string.h>

/* Minimal bounded decimal formatter to avoid depending on libc's snprintf
 * (which drags in newlib reentrancy symbols like _impure_ptr). This is
 * intentionally small and only supports unsigned decimal numbers. */
static size_t append_str(char *buf, size_t buf_sz, size_t pos, const char *s) {
    while (pos + 1 < buf_sz && *s) buf[pos++] = *s++;
    if (pos < buf_sz) buf[pos] = '\0';
    return pos;
}

static size_t append_u32_dec(char *buf, size_t buf_sz, size_t pos, size_t v) {
    char tmp[32];
    int i = 0;
    if (v == 0) {
        if (pos + 1 < buf_sz) buf[pos++] = '0';
        if (pos < buf_sz) buf[pos] = '\0';
        return pos;
    }
    while (v > 0 && i < (int)sizeof(tmp) - 1) {
        tmp[i++] = '0' + (v % 10);
        v /= 10;
    }
    /* reverse */
    while (i-- > 0 && pos + 1 < buf_sz) buf[pos++] = tmp[i];
    if (pos < buf_sz) buf[pos] = '\0';
    return pos;
}

#define BUF_SIZE 128 * 1024
#define PREFETCH_REPEAT 64
#define STRIDE   16
#define RAND_SEED 12345
#define RAND_COUNT 2048

static uint8_t src[BUF_SIZE] __attribute__((aligned(64)));
static uint8_t dst[BUF_SIZE] __attribute__((aligned(64)));
static uint32_t rand_idx[RAND_COUNT];

/*
 * Compile-time bench selector
 *
 * Define any of the BENCH_* symbols on top of the code to select which
 * microbenchmarks to include in a build.
 *
 * If none of the BENCH_* macros are defined, the default is to enable
 * all benches.
 */
#if !defined(BENCH_SEQ) && !defined(BENCH_MEMCPY) \
 && !defined(BENCH_STRIDE_READ) && !defined(BENCH_RANDOM_READ) && !defined(BENCH_COALESCE_WRITE_SB) \
 && !defined(BENCH_COALESCE_WRITE_SW) && !defined(BENCH_FORCE_HIER) && !defined(BENCH_LOAD_THEN_STORE) \
 && !defined(BENCH_STORE_THEN_LOAD) && !defined(BENCH_STAGGERED_FILL) && !defined(BENCH_ALTERNATING_SIZES) \
 && !defined(BENCH_LATE_DATA_CHAIN) && !defined(BENCH_MIXED_STRESS) && !defined(BENCH_REAL_PIXEL) \
 && !defined(BENCH_REAL_PACKET) && !defined(BENCH_REAL_STRUCT) && !defined(BENCH_STRIDED_PREFETCH) \
 && !defined(BENCH_STREAM_READ) && !defined(BENCH_STREAM_PREFETCH) && !defined(BENCH_AMPM_PREFETCH)
#define BENCH_ALL
#endif


static inline uint64_t rdcycle(void) {
    uint64_t cycles;
    asm volatile ("rdcycle %0" : "=r"(cycles));
    return cycles;
}

static void print_result(const char *name, uint64_t cycles, size_t n) {
    uint64_t cB_int = cycles / n;
    uint64_t cB_frac = (cycles * 100) / n - (cB_int * 100);
    // Unified format for all microbenchmarks
    printf("BENCH: %s cycles=%lu bytes=%lu c/B=%lu.%02lu\n",
           name, cycles, (unsigned long)n, cB_int, cB_frac);
}

// Utility: reset dst buffer to the canonical src contents so benches are independent
static void reset_dst(void) {
    for (size_t i = 0; i < BUF_SIZE; i++) dst[i] = src[i];
}

// Verification helpers
#if defined(BENCH_ALL) || defined(BENCH_MEMCPY)
static void verify_memcpy(uint8_t *d, uint8_t *s, size_t n, const char *name) {
    if (memcmp(d, s, n) == 0) printf("VERIFY: PASS - %s memcpy\n", name);
    else printf("VERIFY: FAIL - %s memcpy mismatch\n", name);
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_SEQ)
static void verify_seq_write(uint8_t *d, size_t n) {
    size_t mismatches = 0;
    for (size_t i = 0; i < n; ++i) if (d[i] != (uint8_t)(i & 0xFF)) mismatches++;
    if (mismatches == 0) printf("VERIFY: PASS - seq_write wrote increasing bytes\n");
    else printf("VERIFY: FAIL - seq_write mismatches=%lu\n", mismatches);
}
#endif

// verify_all_byte used by coalesce write and store_then_load benches
#if defined(BENCH_ALL) || defined(BENCH_COALESCE_WRITE_SB) || defined(BENCH_COALESCE_WRITE_SW) || defined(BENCH_STORE_THEN_LOAD)
static void verify_all_byte(uint8_t *d, size_t n, uint8_t v, const char *name) {
    size_t mismatches = 0;
    for (size_t i = 0; i < n; ++i) if (d[i] != v) mismatches++;
    if (mismatches == 0) printf("VERIFY: PASS - %s wrote 0x%02x everywhere\n", name, v);
    else printf("VERIFY: FAIL - %s mismatches=%lu expected=0x%02x\n", name, mismatches, v);
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_STAGGERED_FILL)
static void verify_staggered_fill(uint8_t *d, size_t n) {
    size_t mismatches = 0;
    for (size_t i = 0; i < n; i += 16) {
        if (d[i] != 0) mismatches++;
        if ((i+2) < n && d[i+2] != 0) mismatches++;
        if ((i+4) < n && d[i+4] != 0) mismatches++;
        if ((i+6) < n && d[i+6] != 0) mismatches++;
    }
    if (mismatches == 0) printf("VERIFY: PASS - staggered_fill pattern matches\n");
    else printf("VERIFY: FAIL - staggered_fill mismatches=%lu\n", mismatches);
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_ALTERNATING_SIZES)
static void verify_alternating_sizes(uint8_t *d, size_t n) {
    // Expect the written portion (aligned) to be zero
    uintptr_t base = (uintptr_t)d;
    uintptr_t aligned_base = (base + 7) & ~((uintptr_t)7);
    size_t skip = aligned_base - base;
    size_t total = 0;
    if (n > skip) total = (n - skip) & ~((size_t)7);
    size_t mismatches = 0;
    for (size_t i = 0; i < total; ++i) if (d[skip + i] != 0) mismatches++;
    if (mismatches == 0) printf("VERIFY: PASS - alternating_sizes wrote expected zeros\n");
    else printf("VERIFY: FAIL - alternating_sizes mismatches=%lu\n", mismatches);
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_FORCE_HIER)
static void verify_force_hierarchical(uint8_t *d, size_t n) {
    // The benchmark writes zeros into the aligned region; verify zeros on the touched area
    size_t total = n & ~((size_t)15);
    size_t mismatches = 0;
    for (size_t i = 0; i < total; ++i) if (d[i] != 0) mismatches++;
    if (mismatches == 0) printf("VERIFY: PASS - force_hierarchical_coalesce zeros\n");
    else printf("VERIFY: FAIL - force_hierarchical_coalesce mismatches=%lu\n", mismatches);
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_LATE_DATA_CHAIN)
static void verify_late_data_chain(uint8_t *d, size_t n) {
    size_t mismatches = 0;
    for (size_t i = 0; i < n; ++i) if (d[i] != 0x7F) mismatches++;
    if (mismatches == 0) printf("VERIFY: PASS - late_data_chain all 0x7F\n");
    else printf("VERIFY: FAIL - late_data_chain mismatches=%lu\n", mismatches);
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_MIXED_STRESS)
static void verify_mixed_stress(uint8_t *d, size_t n) {
    // Model the deterministic behavior performed by the inline asm:
    // for i in 0..n-1: idx = rand_idx[i]; d[idx] = 0xA5;
    static uint8_t expect[BUF_SIZE];
    for (size_t i = 0; i < BUF_SIZE; ++i) expect[i] = src[i];
    for (size_t i = 0; i < n; ++i) expect[rand_idx[i]] = 0xA5;
    if (memcmp(d, expect, BUF_SIZE) == 0) printf("VERIFY: PASS - mixed_stress final state matches model\n");
    else printf("VERIFY: FAIL - mixed_stress final state differs from model\n");
}
#endif

// Verify helpers for new real-world benches
#if defined(BENCH_ALL) || defined(BENCH_REAL_PIXEL)
static void verify_rgba(uint8_t *d, size_t n_pixels, const char *name) {
    size_t mismatches = 0;
    for (size_t p = 0; p < n_pixels; ++p) {
        size_t base = p * 4;
        if (d[base + 0] != 0x11) mismatches++;
        if (d[base + 1] != 0x22) mismatches++;
        if (d[base + 2] != 0x33) mismatches++;
        if (d[base + 3] != 0x44) mismatches++;
    }
    if (mismatches == 0) printf("VERIFY: PASS - %s RGBA pattern\n", name);
    else printf("VERIFY: FAIL - %s RGBA mismatches=%lu\n", name, mismatches);
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_REAL_PACKET)
static void verify_packet_header(uint8_t *d, size_t n_packets, const char *name) {
    size_t mismatches = 0;
    for (size_t i = 0; i < n_packets; ++i) {
        size_t b = i * 8;
        // bytes 0..3 should be 0x01 (from sw 0x01010101)
        for (int j = 0; j < 4; ++j) if (d[b + j] != 0x01) mismatches++;
        // sh 0xABCD at offset 4 (little-endian: 0xCD,0xAB)
        if (d[b + 4] != 0xCD) mismatches++;
        if (d[b + 5] != 0xAB) mismatches++;
        // two sb of low-byte 0xCD at offsets 6 and 7
        if (d[b + 6] != 0xCD) mismatches++;
        if (d[b + 7] != 0xCD) mismatches++;
    }
    if (mismatches == 0) printf("VERIFY: PASS - %s packet pattern\n", name);
    else printf("VERIFY: FAIL - %s packet mismatches=%lu\n", name, mismatches);
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_REAL_STRUCT)
static void verify_struct_fields(uint8_t *d, size_t n_elems, const char *name) {
    size_t mismatches = 0;
    for (size_t i = 0; i < n_elems; ++i) {
        size_t b = i * 8;
        // flag at 0
        if (d[b + 0] != 0x7F) mismatches++;
        // padding at 1 (should be zero)
        if (d[b + 1] != 0x00) mismatches++;
        // id sh 0x1234 at offset 2 (little-endian: 0x34,0x12)
        if (d[b + 2] != 0x34) mismatches++;
        if (d[b + 3] != 0x12) mismatches++;
        // value 0xDEADBEEF at offset 4 (little-endian)
        if (d[b + 4] != 0xEF) mismatches++;
        if (d[b + 5] != 0xBE) mismatches++;
        if (d[b + 6] != 0xAD) mismatches++;
        if (d[b + 7] != 0xDE) mismatches++;
    }
    if (mismatches == 0) printf("VERIFY: PASS - %s struct fields\n", name);
    else printf("VERIFY: FAIL - %s struct mismatches=%lu\n", name, mismatches);
}
#endif

// Heavy workload verification: check two guard regions for stray writes
// Heavy workload verification: check two guard regions for stray writes
#if defined(BENCH_ALL) || defined(BENCH_MIXED_STRESS)
static void verify_guard_regions(uint8_t *d, size_t guard_size, const char *name_prefix) {
    // prefix guard
    size_t mismatches = 0;
    for (size_t i = 0; i < guard_size; ++i) if (d[i] != 0xAA) mismatches++;
    if (mismatches == 0) printf("VERIFY: PASS - %s guard intact\n", name_prefix);
    else printf("VERIFY: FAIL - %s guard mismatches=%lu\n", name_prefix, mismatches);
}
#endif

// Verify pattern 1: all bytes should be 0x11
// Verify pattern helpers
#if defined(BENCH_ALL)
static void verify_pattern1(uint8_t *d, size_t count, const char *name) {
    size_t mismatches = 0;
    for (size_t i = 0; i < count; ++i) {
        if (d[i] != 0x11) mismatches++;
    }
    if (mismatches == 0) printf("VERIFY: PASS - %s (0x11 pattern)\n", name);
    else printf("VERIFY: FAIL - %s mismatches=%lu\n", name, mismatches);
}

// Verify pattern 2: all bytes should be 0x22 (from halfword 0x2222)
static void verify_pattern2(uint8_t *d, size_t count, const char *name) {
    size_t mismatches = 0;
    size_t first_mismatch = (size_t)-1;
    // Track a small sample of mismatches for debugging
    const size_t sample_max = 32;
    size_t sample_count = 0;
    struct { size_t idx; uint8_t got; } sample[32];
    // Histogram of mismatches per byte offset within 8-byte blocks
    size_t mod8_hist[8] = {0};

    for (size_t i = 0; i < count; ++i) {
        if (d[i] != 0x22) {
            if (first_mismatch == (size_t)-1) first_mismatch = i;
            mismatches++;
            mod8_hist[i & 7]++;
            if (sample_count < sample_max) {
                sample[sample_count].idx = i;
                sample[sample_count].got = d[i];
                sample_count++;
            }
        }
    }
    if (mismatches == 0) {
        printf("VERIFY: PASS - %s (0x22 pattern)\n", name);
    } else {
        printf("VERIFY: FAIL - %s mismatches=%lu\n", name, mismatches);
        // Print a brief summary to help correlate with coalescing logs
        printf("VERIFY_DEBUG: first_mismatch_at=%lu (offset_mod8=%lu)\n",
               (unsigned long)first_mismatch, (unsigned long)(first_mismatch & 7));
        printf("VERIFY_DEBUG: mismatch_hist_mod8: [0]=%lu [1]=%lu [2]=%lu [3]=%lu [4]=%lu [5]=%lu [6]=%lu [7]=%lu\n",
               (unsigned long)mod8_hist[0], (unsigned long)mod8_hist[1], (unsigned long)mod8_hist[2], (unsigned long)mod8_hist[3],
               (unsigned long)mod8_hist[4], (unsigned long)mod8_hist[5], (unsigned long)mod8_hist[6], (unsigned long)mod8_hist[7]);
        printf("VERIFY_DEBUG: first_%lu_mismatches (idx->got): ", (unsigned long)sample_count);
        for (size_t i = 0; i < sample_count; ++i) {
            printf("%lu->0x%02x%s", (unsigned long)sample[i].idx, sample[i].got, (i + 1 == sample_count) ? "\n" : ", ");
        }
        // Optional: small hex dump around the first mismatch (up to 32 bytes)
        size_t dump_start = (first_mismatch == (size_t)-1) ? 0 : (first_mismatch & ~((size_t)0x1f));
        size_t dump_len = (dump_start + 32 <= count) ? 32 : (count - dump_start);
        printf("VERIFY_DEBUG: hexdump [%lu..%lu): ", (unsigned long)dump_start, (unsigned long)(dump_start + dump_len));
        for (size_t i = 0; i < dump_len; ++i) {
            printf("%02x%s", d[dump_start + i], ((i & 7) == 7) ? " " : "");
        }
        printf("\n");
    }
}

// Verify pattern 3: all bytes should be 0x33 (from word 0x33333333)
static void verify_pattern3(uint8_t *d, size_t count, const char *name) {
    size_t mismatches = 0;
    for (size_t i = 0; i < count; ++i) {
        if (d[i] != 0x33) mismatches++;
    }
    if (mismatches == 0) printf("VERIFY: PASS - %s (0x33 pattern)\n", name);
    else printf("VERIFY: FAIL - %s mismatches=%lu\n", name, mismatches);
}

// Verify pattern 4: mixed pattern within 8B blocks
// offset 0: 0x44, offset 1-2: 0x5555 (LE: 0x55,0x55), offset 3: 0x44, offset 4-7: 0x66666666 (LE: 0x66,0x66,0x66,0x66)
static void verify_pattern4(uint8_t *d, size_t count, const char *name) {
    size_t mismatches = 0;
    for (size_t i = 0; i + 8 <= count; i += 8) {
        if (d[i + 0] != 0x44) mismatches++;
        if (d[i + 1] != 0x55) mismatches++;
        if (d[i + 2] != 0x55) mismatches++;
        if (d[i + 3] != 0x44) mismatches++;
        if (d[i + 4] != 0x66) mismatches++;
        if (d[i + 5] != 0x66) mismatches++;
        if (d[i + 6] != 0x66) mismatches++;
        if (d[i + 7] != 0x66) mismatches++;
    }
    if (mismatches == 0) printf("VERIFY: PASS - %s (mixed pattern)\n", name);
    else printf("VERIFY: FAIL - %s mismatches=%lu\n", name, mismatches);
}

// Verify pattern 5: all bytes should be 0x77
static void verify_pattern5(uint8_t *d, size_t count, const char *name) {
    size_t mismatches = 0;
    for (size_t i = 0; i < count; ++i) {
        if (d[i] != 0x77) mismatches++;
    }
    if (mismatches == 0) printf("VERIFY: PASS - %s (0x77 pattern)\n", name);
    else printf("VERIFY: FAIL - %s mismatches=%lu\n", name, mismatches);
}

// Verify pattern 6: interleaved 0x88/0x99 in 16B blocks
static void verify_pattern6(uint8_t *d, size_t count, const char *name) {
    size_t mismatches = 0;
    for (size_t i = 0; i + 16 <= count; i += 16) {
        // Block A (0-7): 0x88
        for (int j = 0; j < 8; ++j) if (d[i + j] != 0x88) mismatches++;
        // Block B (8-15): 0x99
        for (int j = 8; j < 16; ++j) if (d[i + j] != 0x99) mismatches++;
    }
    if (mismatches == 0) printf("VERIFY: PASS - %s (0x88/0x99 interleaved)\n", name);
    else printf("VERIFY: FAIL - %s mismatches=%lu\n", name, mismatches);
}
#endif

/* heavy_super_stress microbench removed */

/* isolated_pattern2_sh microbench removed */

// Initialize random indices (simple LCG)
static void init_random_indices(void) {
    uint32_t seed = RAND_SEED;
    for (size_t i = 0; i < RAND_COUNT; i++) {
        seed = (1103515245 * seed + 12345);
        rand_idx[i] = seed % BUF_SIZE;
    }
}

#if defined(BENCH_ALL) || defined(BENCH_SEQ)
// Sequential write using inline asm
// Description: write increasing byte values to memory (simple store stream)
void seq_write(uint8_t *buf, size_t n) {
    uint64_t start = rdcycle();
    asm volatile (
        "mv t0, %0\n"      // t0 = buf pointer
        "mv t1, %1\n"      // t1 = n
        "li t2, 0\n"       // counter
        "1:\n"
        "sb t2, 0(t0)\n"   // store lower 8 bits of t2
        "addi t0, t0, 1\n"
        "addi t2, t2, 1\n"
        "addi t1, t1, -1\n"
        "bnez t1, 1b\n"
        :
        : "r"(buf), "r"(n)
        : "t0","t1","t2","memory"
    );
    uint64_t end = rdcycle();
    print_result("seq_write", end - start, n);
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_SEQ)
// Sequential read using inline asm
// Description: read bytes sequentially to measure load throughput
void seq_read(uint8_t *buf, size_t n) {
    volatile uint8_t sink;
    uint64_t start = rdcycle();
    asm volatile (
        "mv t0, %1\n"
        "mv t1, %2\n"
        "1:\n"
        "lb t2, 0(t0)\n"
        "addi t0, t0, 1\n"
        "addi t1, t1, -1\n"
        "bnez t1, 1b\n"
        "mv %0, t2\n"
        : "=r"(sink)
        : "r"(buf), "r"(n)
        : "t0","t1","t2","memory"
    );
    uint64_t end = rdcycle();
    print_result("seq_read", end - start, n);
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_MEMCPY)
// Memcpy using inline asm
// Description: byte-wise copy from src to dst using loads and stores
void memcpy_bench(uint8_t *dst, uint8_t *src, size_t n) {
    uint64_t start = rdcycle();
    asm volatile (
        "mv t0, %0\n" // src
        "mv t1, %1\n" // dst
        "mv t2, %2\n" // n
        "1:\n"
        "lb t3, 0(t0)\n"
        "sb t3, 0(t1)\n"
        "addi t0, t0, 1\n"
        "addi t1, t1, 1\n"
        "addi t2, t2, -1\n"
        "bnez t2, 1b\n"
        :
        : "r"(src), "r"(dst), "r"(n)
        : "t0","t1","t2","t3","memory"
    );
    uint64_t end = rdcycle();
    print_result("memcpy", end - start, n);
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_STRIDE_READ)
// Stride read using inline asm
// Description: read memory with a fixed stride to exercise cache behavior
void stride_read(uint8_t *buf, size_t n, size_t stride) {
    volatile uint8_t sink;
    uint64_t start = rdcycle();
    asm volatile (
        "mv t0, %1\n"        // buf
        "mv t1, %2\n"        // n
        "mv t2, %3\n"        // stride
        "li t3, 0\n"         // index
        "1:\n"
        "lb t4, 0(t0)\n"
        "addi t0, t0, %4\n"
        "addi t3, t3, 1\n"
        "blt t3, t1, 1b\n"
        "mv %0, t4\n"
        : "=r"(sink)
        : "r"(buf), "r"(n/stride), "r"(stride), "i"(STRIDE)
        : "t0","t1","t2","t3","t4","memory"
    );
    uint64_t end = rdcycle();
    print_result("stride_read", end - start, n/stride);
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_RANDOM_READ)
// Random read using inline asm
// Description: perform loads at pseudo-random offsets from buffer
void random_read(uint8_t *buf, size_t n) {
    volatile uint8_t sink;
    uint64_t start = rdcycle();
    asm volatile (
        "mv t0, %1\n"        // buf
        "mv t1, %2\n"        // n
        "mv t2, %3\n"        // rand_idx pointer
        "li t3, 0\n"         // counter
        "1:\n"
        "lw t4, 0(t2)\n"
        "add t5, t0, t4\n"
        "lb t6, 0(t5)\n"
        "addi t2, t2, 4\n"
        "addi t3, t3, 1\n"
        "blt t3, t1, 1b\n"
        "mv %0, t6\n"
        : "=r"(sink)
        : "r"(buf), "r"(n), "r"(rand_idx)
        : "t0","t1","t2","t3","t4","t5","t6","memory"
    );
    uint64_t end = rdcycle();
    print_result("random_read", end - start, n);
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_COALESCE_WRITE_SB)
// Coalesce-friendly byte-wise write test (heavily unrolled)
// Description: write contiguous 16-byte blocks with sb to strongly stress the STQ
void coalesce_write_sb(uint8_t *buf, size_t n) {
    uint64_t start = rdcycle();
    asm volatile (
        "mv t0, %0\n"       // t0 = buf
        "mv t1, %1\n"       // t1 = n (bytes)
        "li t2, 0\n"        // counter (bytes done)
        "1:\n"
        /* write 16 bytes with byte stores to stress coalescing */
        "sb zero, 0(t0)\n"
        "sb zero, 1(t0)\n"
        "sb zero, 2(t0)\n"
        "sb zero, 3(t0)\n"
        "sb zero, 4(t0)\n"
        "sb zero, 5(t0)\n"
        "sb zero, 6(t0)\n"
        "sb zero, 7(t0)\n"
        "sb zero, 8(t0)\n"
        "sb zero, 9(t0)\n"
        "sb zero, 10(t0)\n"
        "sb zero, 11(t0)\n"
        "sb zero, 12(t0)\n"
        "sb zero, 13(t0)\n"
        "sb zero, 14(t0)\n"
        "sb zero, 15(t0)\n"
        "addi t0, t0, 16\n"
        "addi t2, t2, 16\n"
        "blt t2, t1, 1b\n"
        :
        : "r"(buf), "r"(n)
        : "t0","t1","t2","memory"
    );
    // Force the store queue to drain / commit the pending stores so we measure commit cost
    asm volatile ("fence rw, rw" ::: "memory");
    uint64_t end = rdcycle();
    print_result("coalesce_write_sb", end - start, n);
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_COALESCE_WRITE_SW)
// Coalesce-friendly word-based write test (heavily unrolled)
// Description: write contiguous 16-byte blocks using 4 sw per iteration to stress coalescing
void coalesce_write_sw(uint8_t *buf, size_t n) {
    uint64_t start = rdcycle();
    asm volatile (
        "mv t0, %0\n"       // buf
        "mv t1, %1\n"       // n (bytes)
        "li t2, 0\n"        // counter (bytes)
        "li t3, 0x01010101\n" // repeated byte pattern to fill word
        "1:\n"
        // unroll 4 sw stores per iteration (16 bytes)
        "sw t3, 0(t0)\n"
        "sw t3, 4(t0)\n"
        "sw t3, 8(t0)\n"
        "sw t3, 12(t0)\n"
        "addi t0, t0, 16\n"
        "addi t2, t2, 16\n"
        "blt t2, t1, 1b\n"
        :
        : "r"(buf), "r"(n)
        : "t0","t1","t2","t3","memory"
    );
    // force commit/drain so we measure store-commit effects
    asm volatile ("fence rw, rw" ::: "memory");
    uint64_t end = rdcycle();
    print_result("coalesce_write_sw", end - start, n);
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_FORCE_HIER)
// Targeted half-word/word write test to force hierarchical coalescing
// Writes contiguous, aligned half-words (sh) and then words (sw) in a
// pattern that should enable 2->4->8 hierarchical merging in the STQ.
// Description: sequence of half-word and word stores to trigger hierarchical merges
void force_hierarchical_coalesce(uint8_t *buf, size_t n) {
    // Ensure buffer is 16B-aligned base for easy power-of-two alignment
    uintptr_t base = (uintptr_t)buf;
    uintptr_t aligned_base = (base + 15) & ~((uintptr_t)15);
    uint8_t *p = (uint8_t *)aligned_base;

    // We'll write blocks of 16 bytes at a time. Each block we perform
    // 8 half-word stores (2 bytes each) to fully populate the 16B region.
    // Repeat for n bytes (rounded down to multiples of 16).
    size_t total = n & ~((size_t)15);

    uint64_t start = rdcycle();
    asm volatile (
        "mv t0, %0\n"       // t0 = p
        "mv t1, %1\n"       // t1 = total
        "li t2, 0\n"        // outer counter bytes done
        "1:\n"
        // perform 8 half-word stores covering 16 bytes contiguous
        "sh zero, 0(t0)\n"
        "sh zero, 2(t0)\n"
        "sh zero, 4(t0)\n"
        "sh zero, 6(t0)\n"
        "sh zero, 8(t0)\n"
        "sh zero, 10(t0)\n"
        "sh zero, 12(t0)\n"
        "sh zero, 14(t0)\n"
        "addi t0, t0, 16\n"
        "addi t2, t2, 16\n"
        "blt t2, t1, 1b\n"
        :
        : "r"(p), "r"(total)
        : "t0","t1","t2","memory"
    );

    // Now do a second pass using word stores to encourage 4/8 merges
    asm volatile (
        "mv t0, %0\n"       // t0 = p
        "mv t1, %1\n"       // t1 = total
        "li t2, 0\n"        // bytes done
        "1:\n"
        "sw zero, 0(t0)\n"
        "sw zero, 4(t0)\n"
        "sw zero, 8(t0)\n"
        "sw zero, 12(t0)\n"
        "addi t0, t0, 16\n"
        "addi t2, t2, 16\n"
        "blt t2, t1, 1b\n"
        :
        : "r"(p), "r"(total)
        : "t0","t1","t2","memory"
    );

    // Force commit/drain
    asm volatile ("fence rw, rw" ::: "memory");
    uint64_t end = rdcycle();
    print_result("force_hierarchical_coalesce", end - start, total);
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_LOAD_THEN_STORE)
// Load then store: perform a load from each byte then store it back.
// Exercises load->store forwarding/ordering and STQ interactions.
// Description: load then store to exercise forwarding and ordering
void load_then_store(uint8_t *buf, size_t n) {
    uint64_t start = rdcycle();
    asm volatile (
        "mv t0, %0\n"   // buf
        "mv t1, %1\n"   // n
        "li t2, 0\n"    // counter
        "1:\n"
        "lb t3, 0(t0)\n"
        "sb t3, 0(t0)\n"
        "addi t0, t0, 1\n"
        "addi t2, t2, 1\n"
        "blt t2, t1, 1b\n"
        :
        : "r"(buf), "r"(n)
        : "t0","t1","t2","t3","memory"
    );
    asm volatile ("fence rw, rw" ::: "memory");
    uint64_t end = rdcycle();
    print_result("load_then_store", end - start, n);
}
#endif

// Replaced the three "real_*" microbenches with a targeted strided-prefetch
// experiment. The real_pixel_rgba_sb, real_packet_header_mixed and
// real_struct_field_writes benches were removed per request.

// Strided prefetch experiment
// For a set of stride values, perform reads across the buffer at that
// stride and measure cycles per access. This helps observe benefits of
// strided prefetchers which can accelerate regular strided read patterns.
#if defined(BENCH_ALL) || defined(BENCH_STRIDED_PREFETCH)
void strided_prefetch_experiment(uint8_t *buf, size_t n) {
    const size_t strides[] = {256/*, 512, 1024, 2048*/};  // Reduced to key strides only
    const size_t nstrides = sizeof(strides) / sizeof(strides[0]);
    volatile uint8_t sink;

    const int WARMUP_REPEATS = 64; /* unmeasured iterations to let prefetcher learn */

    for (size_t si = 0; si < nstrides; ++si) {
        size_t stride = strides[si];
        size_t count = (n / stride);
        size_t total_count = count * PREFETCH_REPEAT;

        /* Warmup */
        for (int w = 0; w < WARMUP_REPEATS; ++w) {
            if (stride % 4 == 0) {
                asm volatile (
                    "mv t0, %0\n"
                    "mv t1, %1\n"
                    "mv t3, %2\n"
                    "li t2, 0\n"
                    "1:\n"
                    "lw t4, 0(t0)\n"
                    "add t0, t0, t3\n"
                    /* tiny busy-wait to give prefetcher time to issue/arrive */
                    "li t5, 4\n"
                    "2:\n"
                    "addi t5, t5, -1\n"
                    "bnez t5, 2b\n"
                    "addi t2, t2, 1\n"
                    "blt t2, t1, 1b\n"
                    :
                    : "r"(buf), "r"(count), "r"(stride)
                    : "t0","t1","t2","t3","t4","memory"
                );
            } else {
                asm volatile (
                    "mv t0, %0\n"
                    "mv t1, %1\n"
                    "mv t3, %2\n"
                    "li t2, 0\n"
                    "1:\n"
                    "lb t4, 0(t0)\n"
                    "add t0, t0, t3\n"
                    /* tiny busy-wait to give prefetcher time to issue/arrive */
                    "li t5, 4\n"
                    "2:\n"
                    "addi t5, t5, -1\n"
                    "bnez t5, 2b\n"
                    "addi t2, t2, 1\n"
                    "blt t2, t1, 1b\n"
                    :
                    : "r"(buf), "r"(count), "r"(stride)
                    : "t0","t1","t2","t3","t4","memory"
                );
            }
        }

        /* Measured phase */
        uint64_t start = rdcycle();
        for (size_t rep = 0; rep < PREFETCH_REPEAT; ++rep) {
            if (stride % 4 == 0) {
                asm volatile (
                    "mv t0, %1\n"
                    "mv t1, %2\n"
                    "mv t3, %3\n"
                    "li t2, 0\n"
                    "1:\n"
                    "lw t4, 0(t0)\n"
                    "add t0, t0, t3\n"
                    /* tiny busy-wait to give prefetcher time to issue/arrive */
                    "li t5, 4\n"
                    "2:\n"
                    "addi t5, t5, -1\n"
                    "bnez t5, 2b\n"
                    "addi t2, t2, 1\n"
                    "blt t2, t1, 1b\n"
                    "mv %0, t4\n"
                    : "=r"(sink)
                    : "r"(buf), "r"(count), "r"(stride)
                    : "t0","t1","t2","t3","t4","memory"
                );
            } else {
                asm volatile (
                    "mv t0, %1\n"
                    "mv t1, %2\n"
                    "mv t3, %3\n"
                    "li t2, 0\n"
                    "1:\n"
                    "lb t4, 0(t0)\n"
                    "add t0, t0, t3\n"
                    /* tiny busy-wait to give prefetcher time to issue/arrive */
                    "li t5, 4\n"
                    "2:\n"
                    "addi t5, t5, -1\n"
                    "bnez t5, 2b\n"
                    "addi t2, t2, 1\n"
                    "blt t2, t1, 1b\n"
                    "mv %0, t4\n"
                    : "=r"(sink)
                    : "r"(buf), "r"(count), "r"(stride)
                    : "t0","t1","t2","t3","t4","memory"
                );
            }
        }
        uint64_t end = rdcycle();

        char name[64];
        /* build name without snprintf to avoid libc dependencies */
        size_t pos = 0;
        pos = append_str(name, sizeof(name), pos, "strided_prefetch_s");
        pos = append_u32_dec(name, sizeof(name), pos, stride);
        pos = append_str(name, sizeof(name), pos, "_cl");
        if (total_count == 0) total_count = 1;
        uint64_t cycles = end - start;
        uint64_t c_per_cl = cycles / total_count;
        printf("BENCH: %s cycles=%lu cachelines=%lu c/CL=%lu\n",
               name, cycles, (unsigned long)total_count, (unsigned long)c_per_cl);

        /* fence between stride runs (outside timed loops) */
        asm volatile ("fence rw, rw" ::: "memory");
    }

    // optional verify: ensure buffer not modified by the read-only benchmark
    if (memcmp(buf, src, BUF_SIZE) == 0) printf("VERIFY: PASS - strided_prefetch left buffer intact\n");
    else printf("VERIFY: FAIL - strided_prefetch modified buffer\n");
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_STREAM_READ)
// Stream-style read: heavy unrolled word loads to create a sustained stream
void stream_read(uint8_t *buf, size_t n) {
    volatile uint32_t sink = 0;
    // align to 16 bytes for nicer unroll
    size_t total = n & ~((size_t)15);
    uint64_t start = rdcycle();
    asm volatile (
        "mv t0, %1\n"   // buf
        "mv t1, %2\n"   // total
        "li t2, 0\n"    // bytes done
        "1:\n"
        // unroll: 4 lw (16 bytes) per iteration
        "lw t3, 0(t0)\n"
        "lw t4, 4(t0)\n"
        "lw t5, 8(t0)\n"
        "lw t6, 12(t0)\n"
        "addi t0, t0, 16\n"
        "addi t2, t2, 16\n"
        "blt t2, t1, 1b\n"
        "mv %0, t3\n"
        : "=r"(sink)
        : "r"(buf), "r"(total)
        : "t0","t1","t2","t3","t4","t5","t6","memory"
    );
    uint64_t end = rdcycle();
    print_result("stream_read", end - start, total);
}
#endif

// Stream-prefetch experiment: copy strided_prefetch_experiment style but with
// a tight 64B stride to simulate streaming behavior and allow stream
// prefetchers to shine.
#if defined(BENCH_ALL) || defined(BENCH_STREAM_PREFETCH)
void stream_prefetch_experiment(uint8_t *buf, size_t n) {
    const size_t stride = 64 * 3; /* 3 cachelines stride (192 bytes) */
    size_t count = (n / stride);
    volatile uint32_t sink = 0;

    const int WARMUP_REPEATS = 64; /* let prefetcher learn a stream */
    /* Measured repeats tuned so total measured cachelines ~= 65536
     * count = BUF_SIZE/stride (e.g., ~1365), STREAM_REPEAT 48 => total_count ~= 65520
     */
    const int STREAM_REPEAT = 48;  /* measured repeats tuned to match strided total accesses */

    /* Warmup */
    for (int w = 0; w < WARMUP_REPEATS; ++w) {
        if (count == 0) break;
        asm volatile (
            "mv t0, %0\n"   // buf
            "mv t1, %1\n"   // count
            "mv t3, %2\n"   // t3 = stride
            "li t2, 0\n"
            "1:\n"
            "lw t4, 0(t0)\n"
            "add t0, t0, t3\n"
            "addi t2, t2, 1\n"
            "blt t2, t1, 1b\n"
            :
            : "r"(buf), "r"(count), "r"(stride)
            : "t0","t1","t2","t3","t4","memory"
        );
    }

    /* Measured phase */
    uint64_t start = rdcycle();
    for (int rep = 0; rep < STREAM_REPEAT; ++rep) {
        if (count == 0) break;
        asm volatile (
            "mv t0, %1\n"
            "mv t1, %2\n"
            "mv t3, %3\n"   // t3 = stride
            "li t2, 0\n"
            "1:\n"
            "lw t4, 0(t0)\n"
            "add t0, t0, t3\n"
            "addi t2, t2, 1\n"
            "blt t2, t1, 1b\n"
            "mv %0, t3\n"
            : "=r"(sink)
            : "r"(buf), "r"(count), "r"(stride)
            : "t0","t1","t2","t3","memory"
        );
    }
    uint64_t end = rdcycle();

    size_t total_count = count * STREAM_REPEAT;
    if (total_count == 0) total_count = 1;
    uint64_t cycles = end - start;
    uint64_t c_per_cl = cycles / total_count;
        printf("BENCH: stream_prefetch_stride3CL cycles=%lu cachelines=%lu c/CL=%lu\n",
            cycles, (unsigned long)total_count, (unsigned long)c_per_cl);

    /* fence between runs */
    asm volatile ("fence rw, rw" ::: "memory");

    /* verify: ensure read-only */
    if (memcmp(buf, src, BUF_SIZE) == 0) printf("VERIFY: PASS - stream_prefetch left buffer intact\n");
    else printf("VERIFY: FAIL - stream_prefetch modified buffer\n");
}
#endif

/* BENCH_PREFETCH_COMPARE removed: compare_prefetchers was used to compare
 * next-line, strided, and stream prefetchers. The function and its
 * invocation are intentionally removed to reduce test surface area. */

// AMPM prefetch experiment: 
#if defined(BENCH_ALL) || defined(BENCH_AMPM_PREFETCH)
void ampm_prefetch_experiment(uint8_t *buf, size_t n) {
    // AMPM bench uses zone-based offsets (multiples of the cacheline size)
    const size_t zone_sizes[] = {128};
    const size_t nzone_sizes = sizeof(zone_sizes) / sizeof(zone_sizes[0]);
    const size_t offsets[] = {0, 64, 128, 192};
    const size_t noffsets = sizeof(offsets) / sizeof(offsets[0]);
    volatile uint32_t sink = 0;

    const int WARMUP_REPEATS = 64; /* align with strided warmup */
    const int AMPM_REPEAT = 8; /* measured repeats tuned to match strided total accesses */

    for (size_t zi = 0; zi < nzone_sizes; ++zi) {
        size_t zone_size = zone_sizes[zi];
        size_t zone_count = (n / zone_size);
        size_t total_count = zone_count * noffsets * AMPM_REPEAT;

        // Warmup: let AMPM learn per-zone access maps
        for (int w = 0; w < WARMUP_REPEATS; ++w) {
            if (zone_count == 0) break;
            asm volatile (
                "mv t0, %0\n"       // t0 = buf
                "mv t1, %1\n"       // t1 = zone_count
                "li t2, 0\n"        // zone index
                "1:\n"
                "slli t3, t2, 8\n" // t3 = zone_idx * 256
                "add t3, t3, t0\n"  // t3 = zone base
                "lw t4, 0(t3)\n"
                "lw t4, 64(t3)\n"
                "lw t4, 128(t3)\n"
                "lw t4, 192(t3)\n"
                "li t5, 4\n"
                "2:\n"
                "addi t5, t5, -1\n"
                "bnez t5, 2b\n"
                "addi t2, t2, 1\n"
                "blt t2, t1, 1b\n"
                : "=r"(sink)
                : "r"(buf), "r"(zone_count)
                : "t0","t1","t2","t3","t4","t5","memory"
            );
        }

        // Measured phase
        uint64_t start = rdcycle();
        for (int rep = 0; rep < AMPM_REPEAT; ++rep) {
            if (zone_count == 0) break;
            asm volatile (
                "mv t0, %1\n"       // t0 = buf
                "mv t1, %2\n"       // t1 = zone_count
                "li t2, 0\n"        // zone idx
                "1:\n"
                "slli t3, t2, 8\n"
                "add t3, t3, t0\n"
                "lw t4, 0(t3)\n"
                "lw t4, 64(t3)\n"
                "lw t4, 128(t3)\n"
                "lw t4, 192(t3)\n"
                "li t5, 4\n"
                "2:\n"
                "addi t5, t5, -1\n"
                "bnez t5, 2b\n"
                "addi t2, t2, 1\n"
                "blt t2, t1, 1b\n"
                : "=r"(sink)
                : "r"(rep), "r"(buf), "r"(zone_count)
                : "t0","t1","t2","t3","t4","t5","memory"
            );
        }
        uint64_t end = rdcycle();

        char name[64];
        /* build name without snprintf to avoid libc dependencies */
        size_t pos = 0;
        pos = append_str(name, sizeof(name), pos, "ampm_prefetch_zone");
        pos = append_u32_dec(name, sizeof(name), pos, zone_size);
        pos = append_str(name, sizeof(name), pos, "_offs");
        pos = append_u32_dec(name, sizeof(name), pos, noffsets);
        if (total_count == 0) total_count = 1;
        uint64_t cycles = end - start;
        uint64_t c_per_cl = cycles / total_count;
        printf("BENCH: %s cycles=%lu cachelines=%lu c/CL=%lu\n",
               name, cycles, (unsigned long)total_count, (unsigned long)c_per_cl);

        /* fence between zone runs */
        asm volatile ("fence rw, rw" ::: "memory");
    }

    // optional verify: ensure buffer not modified by the read-only benchmark
    if (memcmp(buf, src, BUF_SIZE) == 0) printf("VERIFY: PASS - ampm_prefetch left buffer intact\n");
    else printf("VERIFY: FAIL - ampm_prefetch modified buffer\n");
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_STORE_THEN_LOAD)
// Store then load: store a constant byte to each location then immediately load it.
// Final buffer should contain the constant; this stresses store->load visibility.
// Description: store followed by load to check visibility and STQ ordering
void store_then_load(uint8_t *buf, size_t n) {
    uint64_t start = rdcycle();
    asm volatile (
        "mv t0, %0\n"   // buf
        "mv t1, %1\n"   // n
        "li t2, 0\n"    // counter
        "li t3, 0x5A\n" // pattern
        "1:\n"
        "sb t3, 0(t0)\n"
        "lb t4, 0(t0)\n"
        "addi t0, t0, 1\n"
        "addi t2, t2, 1\n"
        "blt t2, t1, 1b\n"
        :
        : "r"(buf), "r"(n)
        : "t0","t1","t2","t3","t4","memory"
    );
    asm volatile ("fence rw, rw" ::: "memory");
    uint64_t end = rdcycle();
    print_result("store_then_load", end - start, n);
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_MIXED_STRESS)
// Mixed stress: perform randomized loads and stores to stress MSHRs, STQ, and cache
// Description: randomized store/load mix to stress the memory subsystem
void mixed_stress(uint8_t *buf, size_t n) {
    // Ensure random indices are initialized
    init_random_indices();
    uint64_t start = rdcycle();
    asm volatile (
        "mv t0, %0\n"   // buf
        "mv t1, %1\n"   // n (count)
        "mv t2, %2\n"   // rand_idx ptr
        "li t3, 0\n"    // counter
        "1:\n"
        "lw t4, 0(t2)\n"    // index
        "add t5, t0, t4\n"
        "lb t6, 0(t5)\n"    // random load (value ignored)
        "li t6, 0xA5\n"
        "sb t6, 0(t5)\n"    // random store (use t6 for pattern)
        "addi t2, t2, 4\n"
        "addi t3, t3, 1\n"
        "blt t3, t1, 1b\n"
        :
        : "r"(buf), "r"(n), "r"(rand_idx)
        : "t0","t1","t2","t3","t4","t5","t6","memory"
    );
    asm volatile ("fence rw, rw" ::: "memory");
    uint64_t end = rdcycle();
    print_result("mixed_stress", end - start, n);
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_STAGGERED_FILL)
// Corner-case 1: staggered_fill
// Description: sparse writes within cachelines to create partial coalesce masks
// Write bytes at staggered offsets within a cacheline to create partial masks
void staggered_fill(uint8_t *buf, size_t n) {
    // We'll write every 16-byte block but only set bytes 0,2,4,6 (sparsed)
    uint64_t start = rdcycle();
    asm volatile (
        "mv t0, %0\n"    // buf
        "mv t1, %1\n"    // n
        "li t2, 0\n"     // offset
        "1:\n"
        "sb zero, 0(t0)\n"
        "sb zero, 2(t0)\n"
        "sb zero, 4(t0)\n"
        "sb zero, 6(t0)\n"
        "addi t0, t0, 16\n"
        "addi t2, t2, 16\n"
        "blt t2, t1, 1b\n"
        :
        : "r"(buf), "r"(n)
        : "t0","t1","t2","memory"
    );
    asm volatile ("fence rw, rw" ::: "memory");
    uint64_t end = rdcycle();
    print_result("staggered_fill", end - start, n);
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_ALTERNATING_SIZES)
// Corner-case 2: alternating_sizes
// Description: alternate half-word and larger stores to test size-alignment rules
// Alternate writes of different sizes (sb, sh, sw) in adjacent locations
void alternating_sizes(uint8_t *buf, size_t n) {
    // Make deterministic 8B-region fill using four half-word stores per region
    uintptr_t base = (uintptr_t)buf;
    uintptr_t aligned_base = (base + 7) & ~((uintptr_t)7);
    uint8_t *p = (uint8_t *)aligned_base;
    size_t skip = aligned_base - base;
    size_t total = 0;
    if (n > skip) total = (n - skip) & ~((size_t)7);

    uint64_t start = rdcycle();
    asm volatile (
        "mv t0, %0\n"   // p
        "mv t1, %1\n"   // total
        "li t2, 0\n"    // counter
        "1:\n"
        // issue four half-word stores to cover bytes 0..7
        "sh zero, 0(t0)\n"
        "sh zero, 2(t0)\n"
        "sh zero, 4(t0)\n"
        "sh zero, 6(t0)\n"
        "addi t0, t0, 8\n"
        "addi t2, t2, 8\n"
        "blt t2, t1, 1b\n"
        :
        : "r"(p), "r"(total)
        : "t0","t1","t2","memory"
    );
    asm volatile ("fence rw, rw" ::: "memory");
    uint64_t end = rdcycle();
    print_result("alternating_sizes", end - start, n);
}
#endif

#if defined(BENCH_ALL) || defined(BENCH_LATE_DATA_CHAIN)
// Corner-case 3: late_data_chain
// Description: issue addresses first then data later to exercise late data arrivals
// Simulate address issued earlier and data written later by splitting address and data writes
// Do a small loop: first write addresses (stores without data simulation via sb zero), then
// in a second pass write the data to the same addresses; this can exercise coalesce groups
// where some entries become 'is_coalesced' before data arrives.
void late_data_chain(uint8_t *buf, size_t n) {
    // Pass 1: write a marker byte to each target to create store entries
    uint64_t start = rdcycle();
    asm volatile (
        "mv t0, %0\n" // buf
        "mv t1, %1\n" // n
        "li t2, 0\n"
        "1:\n"
        "sb zero, 0(t0)\n"
        "addi t0, t0, 1\n"
        "addi t2, t2, 1\n"
        "blt t2, t1, 1b\n"
        :
        : "r"(buf), "r"(n)
        : "t0","t1","t2","memory"
    );
    // Small busy loop to simulate delay before data arrives
    asm volatile (
        "li t3, 100\n"
        "2:\n"
        "addi t3, t3, -1\n"
        "bnez t3, 2b\n"
        ::: "t3"
    );
    // Pass 2: write actual data
    asm volatile (
        "mv t0, %0\n" // buf
        "mv t1, %1\n" // n
        "li t2, 0\n"
        "1:\n"
        "li t3, 0x7F\n"
        "sb t3, 0(t0)\n"
        "addi t0, t0, 1\n"
        "addi t2, t2, 1\n"
        "blt t2, t1, 1b\n"
        :
        : "r"(buf), "r"(n)
        : "t0","t1","t2","t3","memory"
    );
    asm volatile ("fence rw, rw" ::: "memory");
    uint64_t end = rdcycle();
    print_result("late_data_chain", end - start, n);
}
#endif


int main(void) {
    printf("RISC-V Memory Benchmark\n");

#define INIT_BUFFERS() do { \
    for (size_t i=0;i<256 && i<BUF_SIZE;i++) src[i] = i; \
    for (size_t i=256;i<BUF_SIZE;i+=256) memcpy(&src[i], src, (BUF_SIZE-i)<256?(BUF_SIZE-i):256); \
    init_random_indices(); \
    memcpy(dst, src, BUF_SIZE); \
} while(0)

    INIT_BUFFERS();

#if defined(BENCH_ALL) || defined(BENCH_SEQ)
    reset_dst();
    seq_write(dst, BUF_SIZE);
    #if defined(ENABLE_VERIFY)
        verify_seq_write(dst, BUF_SIZE);
    #endif
    seq_read(dst, BUF_SIZE);
#endif
#if defined(BENCH_ALL) || defined(BENCH_MEMCPY)
    reset_dst();
    memcpy_bench(dst, src, BUF_SIZE);
    #if defined(ENABLE_VERIFY)
        verify_memcpy(dst, src, BUF_SIZE, "memcpy_bench");
    #endif
#endif
#if defined(BENCH_ALL) || defined(BENCH_STRIDE_READ)
    reset_dst();
    stride_read(dst, BUF_SIZE, STRIDE);
    #if defined(ENABLE_VERIFY)
        if (memcmp(dst, src, BUF_SIZE) == 0) printf("VERIFY: PASS - stride_read left buffer intact\n");
        else printf("VERIFY: FAIL - stride_read modified buffer\n");
    #endif
#endif
#if defined(BENCH_ALL) || defined(BENCH_RANDOM_READ)
    reset_dst();
    random_read(dst, RAND_COUNT);
    #if defined(ENABLE_VERIFY)
        if (memcmp(dst, src, BUF_SIZE) == 0) printf("VERIFY: PASS - random_read left buffer intact\n");
        else printf("VERIFY: FAIL - random_read modified buffer\n");
    #endif
#endif
#if defined(BENCH_ALL) || defined(BENCH_COALESCE_WRITE_SB)
    reset_dst();
    coalesce_write_sb(dst, BUF_SIZE);
    // coalesce_write_sb writes zero bytes; verify
    #if defined(ENABLE_VERIFY)
        verify_all_byte(dst, BUF_SIZE, 0x00, "coalesce_write_sb");
    #endif
#endif
#if defined(BENCH_ALL) || defined(BENCH_COALESCE_WRITE_SW)
    reset_dst();
    coalesce_write_sw(dst, BUF_SIZE);
    // expected pattern is 0x01 per byte because word 0x01010101 was stored
    #if defined(ENABLE_VERIFY)
        verify_all_byte(dst, BUF_SIZE, 0x01, "coalesce_write_sw");
    #endif
#endif
#if defined(BENCH_ALL) || defined(BENCH_FORCE_HIER)
    reset_dst();
    force_hierarchical_coalesce(dst, BUF_SIZE);
    #if defined(ENABLE_VERIFY)
        verify_force_hierarchical(dst, BUF_SIZE);
    #endif
#endif
#if defined(BENCH_ALL) || defined(BENCH_LOAD_THEN_STORE)
    reset_dst();
    load_then_store(dst, BUF_SIZE);
    #if defined(ENABLE_VERIFY)
        printf("VERIFY: load_then_store completed (no content assert)\n");
        // optional: sample-check that buffer still contains bytes (no strict assert)
        if (memcmp(dst, src, BUF_SIZE) != 0) printf("VERIFY: load_then_store changed buffer (expected behavior)\n");
        else printf("VERIFY: load_then_store buffer unchanged\n");
    #endif
#endif
#if defined(BENCH_ALL) || defined(BENCH_STORE_THEN_LOAD)
    reset_dst();
    store_then_load(dst, BUF_SIZE);
    #if defined(ENABLE_VERIFY)
        verify_all_byte(dst, BUF_SIZE, 0x5A, "store_then_load");
    #endif
#endif
    // Coalescing corner-cases (run BEFORE mixed_stress so they are not overwritten)
#if defined(BENCH_ALL) || defined(BENCH_STAGGERED_FILL)
    reset_dst();
    staggered_fill(dst, BUF_SIZE);
    #if defined(ENABLE_VERIFY)
        verify_staggered_fill(dst, BUF_SIZE);
    #endif
#endif

#if defined(BENCH_ALL) || defined(BENCH_ALTERNATING_SIZES)
    reset_dst();
    alternating_sizes(dst, BUF_SIZE);
    #if defined(ENABLE_VERIFY)
        verify_alternating_sizes(dst, BUF_SIZE);
    #endif
#endif

#if defined(BENCH_ALL) || defined(BENCH_LATE_DATA_CHAIN)
    reset_dst();
    late_data_chain(dst, BUF_SIZE);
    #if defined(ENABLE_VERIFY)
        verify_late_data_chain(dst, BUF_SIZE);
    #endif
#endif

#if defined(BENCH_ALL) || defined(BENCH_MIXED_STRESS)
    reset_dst();
    mixed_stress(dst, RAND_COUNT);
    #if defined(ENABLE_VERIFY)
        verify_mixed_stress(dst, RAND_COUNT);
    #endif
#endif
/* bench_heavy_super_stress and bench_isolated_pattern2 removed */
// Strided prefetch experiment: measure several stride sizes to observe
// whether a strided prefetcher accelerates regular strided reads.
#if defined(BENCH_ALL) || defined(BENCH_STRIDED_PREFETCH)
    reset_dst();
    strided_prefetch_experiment(dst, BUF_SIZE);
    #if defined(ENABLE_VERIFY)
        /* The experiment performs read-only accesses; verify buffer intact. */
        int sp_cmp = memcmp(dst, src, BUF_SIZE);
        if (sp_cmp == 0) {
            printf("VERIFY: PASS - strided_prefetch left buffer intact\n");
        } else {
            /* Print a small window of mismatching bytes to help debug corruption. */
            int first_diff = -1;
            for (int i = 0; i < BUF_SIZE; i++) {
                if (dst[i] != src[i]) { first_diff = i; break; }
            }
            printf("VERIFY: FAIL - strided_prefetch modified buffer (memcmp=%d, first_diff=%d, dst_base=%p, first_addr=%p)\n",
                   sp_cmp, first_diff, (void *)dst, (void *)&dst[first_diff]);
            if (first_diff >= 0) {
                int start = first_diff - 16;
                if (start < 0) start = 0;
                int end = first_diff + 16;
                if (end > BUF_SIZE) end = BUF_SIZE;
                printf("  Window around first_diff (offset %d to %d):\n", start, end-1);
                for (int i = start; i < end; i++) {
                    printf("    off=%5d src=0x%02x dst=0x%02x\n", i, (unsigned char)src[i], (unsigned char)dst[i]);
                }
            }
        }
    #endif
#endif

#if defined(BENCH_ALL) || defined(BENCH_AMPM_PREFETCH)
    reset_dst();
    ampm_prefetch_experiment(dst, BUF_SIZE);
    #if defined(ENABLE_VERIFY)
        if (memcmp(dst, src, BUF_SIZE) == 0) printf("VERIFY: PASS - ampm_prefetch left buffer intact\n");
        else printf("VERIFY: FAIL - ampm_prefetch modified buffer\n");
    #endif
#endif

/* BENCH_PREFETCH_COMPARE block removed: compare_prefetchers call no longer present. */

#if defined(BENCH_ALL) || defined(BENCH_STREAM_PREFETCH)
    reset_dst();
    stream_prefetch_experiment(dst, BUF_SIZE);
    #if defined(ENABLE_VERIFY)
        if (memcmp(dst, src, BUF_SIZE) == 0) printf("VERIFY: PASS - stream_prefetch left buffer intact\n");
        else printf("VERIFY: FAIL - stream_prefetch modified buffer\n");
    #endif
#endif

#if defined(BENCH_ALL) || defined(BENCH_STREAM_READ)
    reset_dst();
    stream_read(dst, BUF_SIZE);
#endif
    // Summary: mixed_stress verification already printed above
    // Done.
    printf("DONE\n");
    return 0;
}
