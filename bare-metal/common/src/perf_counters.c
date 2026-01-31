#include <stdio.h>
#include "perf_counters.h"

static volatile uint64_t* const l2_ctrl = (volatile uint64_t*)L2_CTRL_BASE;

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

// Event Masks
#define SET_2 2
#define EVENT_D_MISS      ((1 << 1) << 8 | SET_2)
#define EVENT_STORE_MISS  ((1 << 6) << 8 | SET_2)
#define EVENT_D_HIT       ((1 << 7) << 8 | SET_2)
#define EVENT_SB_HIT      ((1 << 8) << 8 | SET_2)
#define EVENT_MSHR_ALLOC  ((1 << 9) << 8 | SET_2)
#define EVENT_PF_ISSUED   ((1 << 10) << 8 | SET_2)
#define EVENT_PF_USED     ((1 << 11) << 8 | SET_2)

#define write_csr(csr, val) asm volatile ("csrw %0, %1" :: "i"(csr), "r"((unsigned long)(val)))
#define read_csr(csr) ({ unsigned long __tmp; asm volatile ("csrr %0, %1" : "=r"(__tmp) : "i"(csr)); __tmp; })

void configure_events(void) {
    write_csr(CSR_MHPMEVENT3, EVENT_D_HIT);
    write_csr(CSR_MHPMEVENT4, EVENT_D_MISS);
    write_csr(CSR_MHPMEVENT5, EVENT_SB_HIT);
    write_csr(CSR_MHPMEVENT6, EVENT_MSHR_ALLOC);
    write_csr(CSR_MHPMEVENT7, EVENT_PF_ISSUED);
    write_csr(CSR_MHPMEVENT8, EVENT_PF_USED);
}

void read_l2_counters(l2_counters_t* c) {
    c->hits        = l2_ctrl[L2_HITS_OFFSET / 8];
    c->misses      = l2_ctrl[L2_MISSES_OFFSET / 8];
    c->mshr_allocs = l2_ctrl[MSHR_ALLOCS_OFFSET / 8];
    c->pf_issued   = l2_ctrl[L2_PF_ISSUED_OFFSET / 8];
    c->pf_used     = l2_ctrl[L2_PF_USED_OFFSET / 8];
}

void read_l1_counters(l1_counters_t* c) {
    c->d_hits    = read_csr(CSR_MHPMCOUNTER3);
    c->d_misses  = read_csr(CSR_MHPMCOUNTER4);
    c->sb_hits   = read_csr(CSR_MHPMCOUNTER5);
    c->mshr_allocs = read_csr(CSR_MHPMCOUNTER6);
    c->pf_issued = read_csr(CSR_MHPMCOUNTER7);
    c->pf_used   = read_csr(CSR_MHPMCOUNTER8);
}

void print_l2_stats(const char* label, l2_counters_t* start, l2_counters_t* end) {
    printf("[%s] L2 Stats:\n", label);
    printf("  Hits:        %lu\n", end->hits - start->hits);
    printf("  Misses:      %lu\n", end->misses - start->misses);
    printf("  MSHR Allocs: %lu\n", end->mshr_allocs - start->mshr_allocs);
    printf("  PF Issued:   %lu\n", end->pf_issued - start->pf_issued);
    printf("  PF Used:     %lu\n", end->pf_used - start->pf_used);
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
