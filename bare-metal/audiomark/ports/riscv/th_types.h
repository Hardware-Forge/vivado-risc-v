/* RISC-V Audiomark type mappings
 * Provide the TH_* instance typedefs expected by the benchmark.
 * We map the real/complex FFT instance types to small wrappers that
 * hold pointers to kiss_fft allocations. This keeps the rest of the
 * code unchanged while allowing us to call kiss_fft/kiss_fftr.
 */

#ifndef __TH_TYPES_RISCV_H
#define __TH_TYPES_RISCV_H

#include <stdint.h>
#include <stdlib.h>
#include "lib/speexdsp/libspeexdsp/kiss_fft.h"
#include "lib/speexdsp/libspeexdsp/kiss_fftr.h"

#ifdef __cplusplus
extern "C" {
#endif

#define TH_FLOAT32_TYPE float

typedef struct {
    kiss_fft_cfg cfg; /* complex FFT cfg */
    int nfft;
} th_cfft_instance_f32_t;

typedef struct {
    void *cfg; /* opaque - use kiss_fftr_alloc returned pointer */
    int nfft;
} th_rfft_instance_f32_t;

/* Exported macro names expected by ee_types.h */
#define TH_RFFT_INSTANCE_FLOAT32_TYPE th_rfft_instance_f32_t
#define TH_CFFT_INSTANCE_FLOAT32_TYPE th_cfft_instance_f32_t
#define TH_MATRIX_INSTANCE_FLOAT32_TYPE void /* not used directly here */

#ifdef __cplusplus
}
#endif

#endif /* __TH_TYPES_RISCV_H */
