/*
 * CoreMark-Pro Combined Harness for Bare-Metal
 * Runs all workloads sequentially and calculates final score.
 */

#include "th_lib.h"
#include "mith_workload.h"
#include "al_smp.h"
#include "perf_counters.h"

static inline uint64_t rdinstret(void) {
    uint64_t instret;
     asm volatile ("rdinstret %0" : "=r"(instret));
    return instret;
}

/* Prototypes for workload entry points (renamed via -Dmain=run_...) */
extern int run_core(int argc, char *argv[]);
extern int run_cjpeg(int argc, char *argv[]);
extern int run_linear(int argc, char *argv[]);
extern int run_loops(int argc, char *argv[]);
extern int run_nnet(int argc, char *argv[]);
extern int run_parser(int argc, char *argv[]);
extern int run_radix2(int argc, char *argv[]);
extern int run_sha(int argc, char *argv[]);
extern int run_zip(int argc, char *argv[]);

/* Exported score from mith_lib.c */
extern double g_last_score;

/* Reference scores and scale factors from README */
typedef struct {
    const char *name;
    int (*run_func)(int, char*[]);
    double ref_score;
    double scale_factor;
} workload_info_t;

workload_info_t workloads[] = {
    {"cjpeg-rose7-preset",      run_cjpeg,   40.3438,  1.0},
    {"core",                    run_core,    2855.0,   10000.0},
    {"linear_alg-mid-100x100-sp", run_linear, 38.5624, 1.0},
    {"loops-all-mid-10k-sp",    run_loops,   0.87959,  1.0},
    {"nnet_test",               run_nnet,    1.45853,  1.0},
    {"parser-125k",             run_parser,  4.81116,  1.0},
    {"radix2-big-64k",          run_radix2,  99.6587,  1.0},
    {"sha-test",                run_sha,     48.5201,  1.0},
    {"zip-test",                run_zip,     21.3618,  1.0}
};

#define NUM_WORKLOADS (sizeof(workloads)/sizeof(workload_info_t))

/* Simple math implementations to avoid libm linking issues */
double simple_ln(double x) {
    if (x <= 0) return 0; // Error
    double sum = 0;
    double y = (x - 1) / (x + 1);
    double y2 = y * y;
    double num = y;
    double den = 1;
    int i;
    for (i = 0; i < 10; i++) { // 10 iterations enough for this
        sum += num / den;
        num *= y2;
        den += 2;
    }
    return 2 * sum;
}

double simple_exp(double x) {
    double sum = 1.0;
    double term = 1.0;
    int i;
    for (i = 1; i < 20; i++) {
        term *= x / i;
        sum += term;
    }
    return sum;
}

int main(int argc, char *argv[]) {
    int i;
    double sum_logs = 0.0;
    double score, result;
    char *internal_argv[] = {"app", "-v0"}; /* Disable verification for performance run */
    int internal_argc = 2;

    al_main(argc, argv);
    configure_events();
    th_printf("\r\n*** CoreMark-PRO Combined Benchmark Started ***\r\n");

    for (i = 0; i < NUM_WORKLOADS; i++) {
        th_printf("\r\n>>> Running Workload: %s\r\n", workloads[i].name);
        
    /* Run workload */
        uint64_t start_cycles = rdcycle();
        uint64_t start_instret = rdinstret();
        l1_counters_t l1_start, l1_end;
        l2_counters_t l2_start, l2_end;
        read_l1_counters(&l1_start);
        read_l2_counters(&l2_start);

        workloads[i].run_func(internal_argc, internal_argv);

        read_l2_counters(&l2_end);
        read_l1_counters(&l1_end);
        uint64_t end_instret = rdinstret();
        uint64_t end_cycles = rdcycle();
        
        /* Capture score */
        score = g_last_score;
        
        /* Compute term: (score / ref) * scale */
        result = (score / workloads[i].ref_score) * workloads[i].scale_factor;
        
        th_printf(">>> Score: %.4f Iter/s | Normalized: %.4f\r\n", score, result);

        /* Print Performance Counters */
        uint64_t cycles = end_cycles - start_cycles;
        uint64_t instret = end_instret - start_instret;
        th_printf("\n[PERF] %s Performance:\n", workloads[i].name);
        th_printf("  Cycles:      %lu\n", cycles);
        th_printf("  Instret:     %lu\n", instret);
        th_printf("  CPI:         %.2f\n", (double)cycles / instret);
        print_l1_stats(workloads[i].name, &l1_start, &l1_end);
        print_l2_stats(workloads[i].name, &l2_start, &l2_end);
        th_printf("\n");
        
        /* Accumulate log for geometric mean */
        sum_logs += simple_ln(result);
    }

    /* Final Score = exp(mean(logs)) * 1000 */
    double final_score = simple_exp(sum_logs / NUM_WORKLOADS) * 1000.0;

    th_printf("\r\n==============================================\r\n");
    th_printf("CoreMark-PRO Final Score: %.4f\r\n", final_score);
    th_printf("==============================================\r\n");

    return 0;
}
