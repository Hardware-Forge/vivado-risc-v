/* Bare-metal timer using RISC-V cycle counter.
 * Returns seconds as a double. Uses FPGA_CPU_CLK_FREQ (MHz) defined
 * by the build system (common.mk passes -DFPGA_CPU_CLK_FREQ=...).
 */
#include <stdint.h>

static inline uint64_t rdcycle64(void) {
    uint64_t v;
    asm volatile ("rdcycle %0" : "=r"(v));
    return v;
}

double mysecond()
{
    uint64_t cycles = rdcycle64();
    /* FPGA_CPU_CLK_FREQ is MHz, so cycles / (MHz * 1e6) = seconds */
    double secs = (double)cycles / ((double)FPGA_CPU_CLK_FREQ * 1e6);
    return secs;
}

double mysecond_() { return mysecond(); }
