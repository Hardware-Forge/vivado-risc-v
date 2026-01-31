/* Minimal bare-metal main for Audiomark on RISC-V. Calls the benchmark
 * initialization and runs multiple iterations to compute a score.
 */

#include "ee_audiomark.h"
#include <stdio.h>
#include "perf_counters.h"

/* Forward-declare kprintf since we use -Dprintf=kprintf */
int kprintf(const char *fmt, ...);



/* CPU frequency in Hz - defined by build system as FPGA_CPU_CLK_FREQ (in MHz) */
#ifndef FPGA_CPU_CLK_FREQ
#define FPGA_CPU_CLK_FREQ 100
#endif
#define CPU_HZ ((unsigned long)FPGA_CPU_CLK_FREQ * 1000000UL)

int main(void)
{
    unsigned long start, end, cycles;
    unsigned int iterations = 10;  /* Start with 1 iteration */
    
    kprintf("Audiomark RISC-V bare-metal\n");
    kprintf("CPU frequency: %d MHz\n", FPGA_CPU_CLK_FREQ);

    /* Initialize benchmark components */
    if (ee_audiomark_initialize()) {
        kprintf("Audiomark init failed\n");
        return -1;
    }

    kprintf("Running %d iteration(s)...\n", iterations);

    /* Timed benchmark run */
    /* Timed benchmark run */
    l1_counters_t l1_start, l1_end;
    l2_counters_t l2_start, l2_end;
    configure_events();
    read_l1_counters(&l1_start);
    read_l2_counters(&l2_start);

    start = rdcycle();
    for (unsigned int i = 0; i < iterations; i++) {
        if (ee_audiomark_run()) {
            kprintf("Audiomark run failed at iteration %d\n", i);
            return -1;
        }
        kprintf("Iteration %d complete\n", i + 1);
    }
    end = rdcycle();
    read_l1_counters(&l1_end);
    read_l2_counters(&l2_end);
    cycles = end - start;

    kprintf("\n=== Audiomark Results ===\n");
    kprintf("Total cycles: %lu\n", cycles);
    
    /* Compute time in milliseconds to avoid overflow */
        float total_seconds = (float)cycles / (float)CPU_HZ;
        float score = (float)iterations / total_seconds * 1000.0f * (1.0f / 1.5f);
        float score_per_mhz = score / (float)FPGA_CPU_CLK_FREQ;

        kprintf("Total time: %f seconds\n", (double)total_seconds);
        kprintf("Score: %f AudioMarks\n", (double)score);
        kprintf("Score/MHz: %f AudioMarks/MHz\n", (double)score_per_mhz);

    print_l1_stats("AudioMark", &l1_start, &l1_end);
    print_l2_stats("AudioMark", &l2_start, &l2_end);

    kprintf("Done.\n");
    return 0;
}
