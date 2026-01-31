/* Minimal FFT adapter for SpeexDSP expectations when building for RISC-V.
 * Implements spx_fft_init/destroy/fft/ifft wrappers using kiss_fftr.
 */

#include "lib/speexdsp/libspeexdsp/fftwrap.h"
#include "lib/speexdsp/libspeexdsp/kiss_fft.h"
#include "lib/speexdsp/libspeexdsp/kiss_fftr.h"
#include <stdlib.h>

struct riscv_fft {
    kiss_fftr_cfg forward;
    kiss_fftr_cfg backward;
    int N;
};

void *spx_fft_init(int size)
{
    struct riscv_fft *t = malloc(sizeof(*t));
    if (!t) return NULL;
    size_t mem = 0;
    t->forward = kiss_fftr_alloc(size, 0, NULL, &mem);
    if (!t->forward) {
        void *m = malloc(mem);
        if (!m) { free(t); return NULL; }
        t->forward = kiss_fftr_alloc(size, 0, m, &mem);
    }
    mem = 0;
    t->backward = kiss_fftr_alloc(size, 1, NULL, &mem);
    if (!t->backward) {
        void *m = malloc(mem);
        if (!m) { free(t->forward); free(t); return NULL; }
        t->backward = kiss_fftr_alloc(size, 1, m, &mem);
    }
    t->N = size;
    return t;
}

void spx_fft_destroy(void *table)
{
    struct riscv_fft *t = (struct riscv_fft *)table;
    if (!t) return;
    free(t->forward);
    free(t->backward);
    free(t);
}

/* In floating-point mode spx_word16_t is float; provide float variants */
void spx_fft_float(void *table, float *in, float *out)
{
    struct riscv_fft *t = (struct riscv_fft *)table;
    if (!t) return;
    kiss_fftr(t->forward, in, (kiss_fft_cpx *)out);
}

void spx_ifft_float(void *table, float *in, float *out)
{
    struct riscv_fft *t = (struct riscv_fft *)table;
    if (!t) return;
    kiss_fftri(t->backward, (kiss_fft_cpx *)in, out);
}

/* provide entry points mapping to spx_word16_t signatures when FLOATING_POINT
 * is enabled. These simply call the float variants.
 */
void spx_fft(void *table, spx_word16_t *in, spx_word16_t *out)
{
    spx_fft_float(table, (float *)in, (float *)out);
}

void spx_ifft(void *table, spx_word16_t *in, spx_word16_t *out)
{
    spx_ifft_float(table, (float *)in, (float *)out);
}
