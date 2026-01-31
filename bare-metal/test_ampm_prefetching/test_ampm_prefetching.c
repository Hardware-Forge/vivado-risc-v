#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// ============================================================================
// AMPM Prefetcher Benchmark
// Tests zone-based pattern detection capabilities
// ============================================================================

#define BUFFER_SIZE (10 * 1024 * 1024)
#define CACHE_LINE 64
#define PAGE_SIZE 4096

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

// L1 Performance Counters
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

// Test 1: Sequential within zones (AMPM's strength)
void run_test_zone_sequential(const char* name) {
    printf("Starting %s test...\n", name);
    configure_l1_events();
    
    uint64_t sum = 0;
    l2_counters_t l2_start, l2_end;
    l1_counters_t l1_start, l1_end;
    
    read_l2_counters(&l2_start);
    read_l1_counters(&l1_start);
    uint64_t start = rdcycle();
    
    // Access pages sequentially - AMPM should build access maps per zone
    for (int page = 0; page < BUFFER_SIZE / PAGE_SIZE; page++) {
        for (int block = 0; block < PAGE_SIZE / CACHE_LINE; block++) {
            sum += buffer[page * PAGE_SIZE + block * CACHE_LINE];
        }
    }
    
    uint64_t end = rdcycle();
    read_l1_counters(&l1_end);
    read_l2_counters(&l2_end);
    
    uint64_t accesses = BUFFER_SIZE / CACHE_LINE;
    printf("%s Results:\n", name);
    printf("  Total Cycles: %lu\n", end - start);
    printf("  Accesses:     %lu\n", accesses);
    printf("  Cycles/Access: %lu\n", (end - start) / accesses);
    print_l1_stats(name, &l1_start, &l1_end);
    print_l2_stats(name, &l2_start, &l2_end);
    printf("----------------------------------------\n");
}

// Test 2: Random within zones (AMPM should struggle)
void run_test_zone_random(const char* name) {
    printf("Starting %s test...\n", name);
    configure_l1_events();
    
    uint64_t sum = 0;
    l2_counters_t l2_start, l2_end;
    l1_counters_t l1_start, l1_end;
    
    read_l2_counters(&l2_start);
    read_l1_counters(&l1_start);
    uint64_t start = rdcycle();
    
    // Pseudo-random access within each page (no predictable pattern)
    for (int page = 0; page < BUFFER_SIZE / PAGE_SIZE; page++) {
        int offsets[] = {0, 32, 16, 48, 8, 40, 24, 56};
        for (int i = 0; i < 8; i++) {
            sum += buffer[page * PAGE_SIZE + offsets[i] * CACHE_LINE];
        }
    }
    
    uint64_t end = rdcycle();
    read_l1_counters(&l1_end);
    read_l2_counters(&l2_end);
    
    uint64_t accesses = (BUFFER_SIZE / PAGE_SIZE) * 8;
    printf("%s Results:\n", name);
    printf("  Total Cycles: %lu\n", end - start);
    printf("  Accesses:     %lu\n", accesses);
    printf("  Cycles/Access: %lu\n", (end - start) / accesses);
    print_l1_stats(name, &l1_start, &l1_end);
    print_l2_stats(name, &l2_start, &l2_end);
    printf("----------------------------------------\n");
}

int main(void) {
    printf("AMPM Prefetcher Benchmark\n");
    printf("Buffer Size: %d KB\n", BUFFER_SIZE / 1024);
    
    run_test_zone_sequential("Zone Sequential");
    run_test_zone_random("Zone Random");
    
    return 0;
}
