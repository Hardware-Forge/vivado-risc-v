#ifndef PERFORMANCE_COUNTERS_H
#define PERFORMANCE_COUNTERS_H

#include <stdint.h>

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

// L1 Performance Counters
typedef struct {
    uint64_t d_hits;
    uint64_t d_misses;
    uint64_t sb_hits;
    uint64_t mshr_allocs;
    uint64_t pf_issued;
    uint64_t pf_used;
} l1_counters_t;

// Function declarations
void configure_events(void);
void read_l2_counters(l2_counters_t* c);
void read_l1_counters(l1_counters_t* c);
void print_l2_stats(const char* label, l2_counters_t* start, l2_counters_t* end);
void print_l1_stats(const char* label, l1_counters_t* start, l1_counters_t* end);

static inline uint64_t rdcycle(void) {
    uint64_t cycles;
    asm volatile ("rdcycle %0" : "=r"(cycles));
    return cycles;
}

#endif // PERFORMANCE_COUNTERS_H
