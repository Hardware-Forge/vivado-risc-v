#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define BUFFER_SIZE (10 * 1024 * 1024)
#define CACHE_LINE 64

// Large buffer in static memory
volatile uint8_t buffer[BUFFER_SIZE] __attribute__((aligned(CACHE_LINE)));

static inline uint64_t rdcycle(void) {
    uint64_t cycles;
    asm volatile ("rdcycle %0" : "=r"(cycles));
    return cycles;
}

// L2 Performance Counters
#define L2_CTRL_BASE         0x2010000
#define L2_HITS_OFFSET       0x100
#define L2_MISSES_OFFSET     0x108
#define MSHR_ALLOCS_OFFSET   0x110
#define L2_PF_ISSUED_OFFSET  0x118
#define L2_PF_USED_OFFSET    0x120

typedef struct {
    uint64_t hits;
    uint64_t misses;
    uint64_t mshr_allocs;
    uint64_t pf_issued;
    uint64_t pf_used;
} l2_counters_t;

static volatile uint64_t* const l2_ctrl = (volatile uint64_t*)L2_CTRL_BASE;

void read_l2_counters(l2_counters_t* c) {
    c->hits        = *(volatile uint64_t*)(L2_CTRL_BASE + L2_HITS_OFFSET);
    c->misses      = *(volatile uint64_t*)(L2_CTRL_BASE + L2_MISSES_OFFSET);
    c->mshr_allocs = *(volatile uint64_t*)(L2_CTRL_BASE + MSHR_ALLOCS_OFFSET);
    c->pf_issued   = *(volatile uint64_t*)(L2_CTRL_BASE + L2_PF_ISSUED_OFFSET);
    c->pf_used     = *(volatile uint64_t*)(L2_CTRL_BASE + L2_PF_USED_OFFSET);
}

void print_l2_stats(const char* label, l2_counters_t* start, l2_counters_t* end) {
    printf("[%s] L2 Stats:\n", label);
    printf("  Hits:        %lu\n", end->hits - start->hits);
    printf("  Misses:      %lu\n", end->misses - start->misses);
    printf("  MSHR Allocs: %lu\n", end->mshr_allocs - start->mshr_allocs);
    printf("  PF Issued:   %lu\n", end->pf_issued - start->pf_issued);
    printf("  PF Used:     %lu\n", end->pf_used - start->pf_used);
}

// ============================================================================
// L1 Performance Counters (CSR-based)
// ============================================================================

// CSR Access Macros
#define CSR_MHPMEVENT3  0x323
#define CSR_MHPMEVENT4  0x324
#define CSR_MHPMEVENT5  0x325
#define CSR_MHPMEVENT6  0x326
#define CSR_MHPMEVENT7  0x327
#define CSR_MHPMEVENT8  0x328

#define CSR_MHPMCOUNTER3 0xB03
#define CSR_MHPMCOUNTER4 0xB04
#define CSR_MHPMCOUNTER5 0xB05
#define CSR_MHPMCOUNTER6 0xB06
#define CSR_MHPMCOUNTER7 0xB07
#define CSR_MHPMCOUNTER8 0xB08

// Event Masks (Based on BoomCore.scala append order)
// eventSel = (Mask << 8) | SetID
// Set 2: D$ events (see Boom perfEvents definition)
#define SET_2 2
#define EVENT_D_MISS      ((1 << 1) << 8 | SET_2)
#define EVENT_D_HIT       ((1 << 7) << 8 | SET_2)
#define EVENT_SB_HIT      ((1 << 8) << 8 | SET_2)
#define EVENT_MSHR_ALLOC  ((1 << 9) << 8 | SET_2)
#define EVENT_PF_ISSUED   ((1 << 10) << 8 | SET_2)
#define EVENT_PF_USED     ((1 << 11) << 8 | SET_2)

#define write_csr(csr, val) asm volatile ("csrw %0, %1" :: "i"(csr), "r"((unsigned long)(val)))
#define read_csr(csr) ({ unsigned long __tmp; asm volatile ("csrr %0, %1" : "=r"(__tmp) : "i"(csr)); __tmp; })

void configure_l1_events() {
    write_csr(CSR_MHPMEVENT3, EVENT_D_HIT);
    write_csr(CSR_MHPMEVENT4, EVENT_D_MISS); 
    write_csr(CSR_MHPMEVENT5, EVENT_SB_HIT);
    write_csr(CSR_MHPMEVENT6, EVENT_MSHR_ALLOC);
    write_csr(CSR_MHPMEVENT7, EVENT_PF_ISSUED);
    write_csr(CSR_MHPMEVENT8, EVENT_PF_USED);
}

typedef struct {
    uint64_t d_hits;
    uint64_t d_misses;
    uint64_t sb_hits;
    uint64_t mshr_allocs;
    uint64_t pf_issued;
    uint64_t pf_used;
} l1_counters_t;

void read_l1_counters(l1_counters_t* c) {
    c->d_hits    = read_csr(CSR_MHPMCOUNTER3);
    c->d_misses  = read_csr(CSR_MHPMCOUNTER4);
    c->sb_hits   = read_csr(CSR_MHPMCOUNTER5);
    c->mshr_allocs = read_csr(CSR_MHPMCOUNTER6);
    c->pf_issued = read_csr(CSR_MHPMCOUNTER7);
    c->pf_used   = read_csr(CSR_MHPMCOUNTER8);
}

void print_l1_stats(const char* label, l1_counters_t* start, l1_counters_t* end) {
    printf("[%s] L1 Stats:\n", label);
    printf("  D$ Hits:     %lu\n", end->d_hits - start->d_hits);
    printf("  D$ Misses:   %lu\n", end->d_misses - start->d_misses);
    printf("  SB Hits:     %lu\n", end->sb_hits - start->sb_hits);
    printf("  MSHR Allocs: %lu\n", end->mshr_allocs - start->mshr_allocs);
    printf("  PF Issued:   %lu\n", end->pf_issued - start->pf_issued);
    printf("  PF Used:     %lu\n", end->pf_used - start->pf_used);
}

void run_test_stride(int stride, const char* name) {
    printf("Starting %s test (stride=%d bytes)...\n", name, stride);
    
    configure_l1_events();
    
    // Simple verification sum to prevent optimization
    uint64_t sum = 0;
    
    l2_counters_t l2_start, l2_end;
    l1_counters_t l1_start, l1_end;
    
    read_l2_counters(&l2_start);
    read_l1_counters(&l1_start);
    uint64_t start = rdcycle();
    
    for (int i = 0; i < BUFFER_SIZE; i += stride) {
        sum += buffer[i];
    }
    
    uint64_t end = rdcycle();
    read_l1_counters(&l1_end);
    read_l2_counters(&l2_end);
    
    uint64_t total_cycles = end - start;
    uint64_t accesses = BUFFER_SIZE / stride;
    
    if (sum == 0xDEADBEEF) printf("Sum: %lu\n", sum);
    
    printf("%s Results:\n", name);
    printf("  Total Cycles: %lu\n", total_cycles);
    printf("  Accesses:     %lu\n", accesses);
    printf("  Cycles/Access: %lu\n", accesses ? total_cycles / accesses : 0);
    print_l1_stats(name, &l1_start, &l1_end);
    print_l2_stats(name, &l2_start, &l2_end);
    printf("----------------------------------------\n");
}

void run_test_interleaved(const char* name) {
    printf("Starting %s test (2 streams, stride=64 bytes)...\n", name);
    
    configure_l1_events();
    
    uint64_t sum = 0;
    int half_buf = BUFFER_SIZE / 2;
    int limit = half_buf;
    
    l2_counters_t l2_start, l2_end;
    l1_counters_t l1_start, l1_end;
    
    read_l2_counters(&l2_start);
    read_l1_counters(&l1_start);
    uint64_t start = rdcycle();
    
    // Interleaved access: A[0], B[0], A[1], B[1]...
    // Stream A starts at 0
    // Stream B starts at offset 128KB
    for (int i = 0; i < limit; i += 64) {
        sum += buffer[i];            // Stream A
        sum += buffer[i + half_buf]; // Stream B
    }
    
    uint64_t end = rdcycle();
    read_l1_counters(&l1_end);
    read_l2_counters(&l2_end);
    
    uint64_t total_cycles = end - start;
    uint64_t accesses = (limit / 64) * 2;
    
    if (sum == 0xDEADBEEF) printf("Sum: %lu\n", sum);
    
    printf("%s Results:\n", name);
    printf("  Total Cycles: %lu\n", total_cycles);
    printf("  Accesses:     %lu\n", accesses);
    printf("  Cycles/Access: %lu\n", accesses ? total_cycles / accesses : 0);
    print_l1_stats(name, &l1_start, &l1_end);
    print_l2_stats(name, &l2_start, &l2_end);
    printf("----------------------------------------\n");
}

int main(void) {
    printf("L2 Strided Prefetcher Benchmark\n");
    printf("Buffer Size: %d KB\n", BUFFER_SIZE / 1024);
    
    // 1. Sequential Access (Baseline for Next-Line & Strided)
    run_test_stride(64, "Sequential (64B)");
    
    // 2. Medium Stride (256B)
    // Next-Line should fail (prefetching +64B is useless for +256B stride)
    // Strided should succeed (learning +256B delta)
    run_test_stride(256, "Medium Stride (256B)");
    
    // 3. Interleaved Streams
    // Checks if the prefetcher can handle two active streams (Associativity)
    run_test_interleaved("Interleaved Streams");
    
    return 0;
}
