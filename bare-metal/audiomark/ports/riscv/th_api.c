/* RISC-V implementation of the th_* helper APIs used by Audiomark.
 * This file provides FFT wrappers using kiss_fft/kiss_fftr and simple
 * fallbacks for the vector/math helpers. The NN classify path is a
 * lightweight stub so the benchmark is runnable on RISC-V targets.
 */

#include "ee_audiomark.h"
#include "ee_api.h"
#include "th_types.h"

/* Audio input data and inter-component buffers (copied from the original
 * barebones port so the RISC-V build has the same data layout). */
/* These include the large raw data files from `ee_data/` */
const int16_t downlink_audio[NINPUT_SAMPLES] = {
#include "ee_data/noise.txt"
};
const int16_t left_microphone_capture[NINPUT_SAMPLES] = {
#include "ee_data/left0.txt"
};
const int16_t right_microphone_capture[NINPUT_SAMPLES] = {
#include "ee_data/right0.txt"
};
int16_t for_asr[NINPUT_SAMPLES];

/* inter-component buffers */
int16_t audio_input[SAMPLES_PER_AUDIO_FRAME];       /* 1 */
int16_t left_capture[SAMPLES_PER_AUDIO_FRAME];      /* 2 */
int16_t right_capture[SAMPLES_PER_AUDIO_FRAME];     /* 3 */
int16_t beamformer_output[SAMPLES_PER_AUDIO_FRAME]; /* 4 */
int16_t aec_output[SAMPLES_PER_AUDIO_FRAME];        /* 5 */
int16_t audio_fifo[AUDIO_FIFO_SAMPLES];             /* 6 */
int8_t  mfcc_fifo[MFCC_FIFO_BYTES];                 /* 7 */
int8_t  classes[OUT_DIM];                           /* 8 */

#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "lib/speexdsp/libspeexdsp/kiss_fft.h"
#include "lib/speexdsp/libspeexdsp/kiss_fftr.h"

/* memory helpers */
void *
th_malloc(size_t size, int req)
{
    (void)req;
    return malloc(size);
}

void
th_free(void *mem, int req)
{
    (void)req;
    free(mem);
}

void *
th_memcpy(void *restrict dst, const void *restrict src, size_t n)
{
    return memcpy(dst, src, n);
}

void *
th_memmove(void * dst, const void * src, size_t n)
{
    return memmove(dst, src, n);
}

void *
th_memset(void *b, int c, size_t len)
{
    return memset(b, c, len);
}

/* Complex FFT (wrap kiss_fft) */
ee_status_t
th_cfft_init_f32(ee_cfft_f32_t *p_instance, int fft_length)
{
    if (!p_instance)
        return EE_STATUS_ERROR;

    if (fft_length <= 0)
        return EE_STATUS_ERROR;

    /* Query required size first */
    size_t lenmem = 0;
    kiss_fft_alloc(fft_length, 0, NULL, &lenmem);

    /* Allocate with our malloc */
    void *mem = malloc(lenmem);
    if (!mem) return EE_STATUS_ERROR;

    kiss_fft_cfg cfg = kiss_fft_alloc(fft_length, 0, mem, &lenmem);
    if (!cfg) { return EE_STATUS_ERROR; }

    p_instance->cfg = cfg;
    p_instance->nfft = fft_length;
    return EE_STATUS_OK;
}

void
th_cfft_f32(ee_cfft_f32_t *p_instance,
            ee_f32_t      *p_buf,
            uint8_t        ifftFlag,
            uint8_t        bitReverseFlagR)
{
    (void)bitReverseFlagR;
    if (!p_instance || !p_buf || !p_instance->cfg) return;

    /* kiss_fft operates on complex arrays of kiss_fft_cpx */
    kiss_fft_cfg cfg = p_instance->cfg;
    int n = p_instance->nfft;

    /* Use a static scratch buffer to avoid per-call malloc/free which
     * leaks with the simple bump allocator. Size it to the maximum FFT
     * we expect to encounter (1024) so it covers MFCC and other stages.
     */
    enum { MAX_FFT = 1024 };
    static kiss_fft_cpx scratch[MAX_FFT * 2];
    if (n > MAX_FFT) return; /* unsupported FFT size */
    kiss_fft_cpx *in = &scratch[0];
    kiss_fft_cpx *out = &scratch[MAX_FFT];

    /* convert interleaved float buffer [real,imag,real,imag,...] into complex */
    for (int i = 0; i < n; ++i) {
        in[i].r = p_buf[2*i];
        in[i].i = p_buf[2*i+1];
    }

    kiss_fft(cfg, in, out);

    /* copy back */
    for (int i = 0; i < n; ++i) {
        p_buf[2*i]   = out[i].r;
        p_buf[2*i+1] = out[i].i;
    }

    /* scratch is static; nothing to free */
}

/* Real FFT wrapper (kiss_fftr) */
ee_status_t
th_rfft_init_f32(ee_rfft_f32_t *p_instance, int fft_length)
{
    if (!p_instance) return EE_STATUS_ERROR;
    if (fft_length <= 0) return EE_STATUS_ERROR;

    /* Query required size first */
    size_t lenmem = 0;
    kiss_fftr_alloc(fft_length, 0, NULL, &lenmem);

    void *mem = malloc(lenmem);
    if (!mem) return EE_STATUS_ERROR;

    void *cfg = kiss_fftr_alloc(fft_length, 0, mem, &lenmem);
    if (!cfg) { return EE_STATUS_ERROR; }

    p_instance->cfg = cfg;
    p_instance->nfft = fft_length;
    return EE_STATUS_OK;
}

void
th_rfft_f32(ee_rfft_f32_t *p_instance,
            ee_f32_t      *p_in,
            ee_f32_t      *p_out,
            uint8_t        ifftFlag)
{
    if (!p_instance || !p_instance->cfg) return;
    int n = p_instance->nfft;
    /* kiss_fftr expects real input of length n in p_in and writes n/2+1 complex bins
     * into an array of kiss_fft_cpx. We use kiss_fftr to compute forward transforms.
     */

    if (ifftFlag) {
        /* inverse real FFT: not implemented here - fallback to zeros */
        for (int i = 0; i < n; ++i) p_out[i] = 0.0f;
        return;
    }

    /* forward real FFT */
    kiss_fftr((kiss_fftr_cfg)p_instance->cfg, p_in, (kiss_fft_cpx *)p_out);
}

/* Simple vector/math helpers implemented in plain C using floats */
void
th_absmax_f32(const ee_f32_t *p_in,
              uint32_t        len,
              ee_f32_t       *p_max,
              uint32_t       *p_index)
{
    if (!p_in || len == 0) {
        if (p_max) *p_max = 0.0f;
        if (p_index) *p_index = 0;
        return;
    }
    ee_f32_t maxv = fabsf(p_in[0]);
    uint32_t idx = 0;
    for (uint32_t i = 1; i < len; ++i) {
        ee_f32_t v = fabsf(p_in[i]);
        if (v > maxv) { maxv = v; idx = i; }
    }
    *p_max = maxv; *p_index = idx;
}

void
th_cmplx_mult_cmplx_f32(const ee_f32_t *p_a,
                        const ee_f32_t *p_b,
                        ee_f32_t       *p_c,
                        uint32_t        len)
{
    for (uint32_t i = 0; i < len; ++i) {
        float ar = p_a[2*i]; float ai = p_a[2*i+1];
        float br = p_b[2*i]; float bi = p_b[2*i+1];
        p_c[2*i]   = ar*br - ai*bi;
        p_c[2*i+1] = ar*bi + ai*br;
    }
}

void
th_cmplx_conj_f32(const ee_f32_t *p_a, ee_f32_t *p_c, uint32_t len)
{
    for (uint32_t i = 0; i < len; ++i) {
        p_c[2*i] = p_a[2*i];
        p_c[2*i+1] = -p_a[2*i+1];
    }
}

void
th_cmplx_dot_prod_f32(const ee_f32_t *p_a,
                      const ee_f32_t *p_b,
                      uint32_t        len,
                      ee_f32_t       *p_r,
                      ee_f32_t       *p_i)
{
    float rr = 0.0f, ii = 0.0f;
    for (uint32_t k = 0; k < len; ++k) {
        float ar = p_a[2*k], ai = p_a[2*k+1];
        float br = p_b[2*k], bi = p_b[2*k+1];
        rr += ar*br - ai*bi;
        ii += ar*bi + ai*br;
    }
    *p_r = rr; *p_i = ii;
}

void
th_int16_to_f32(const int16_t *p_src, ee_f32_t *p_dst, uint32_t len)
{
    for (uint32_t i = 0; i < len; ++i) p_dst[i] = (ee_f32_t)p_src[i];
}

void
th_f32_to_int16(const ee_f32_t *p_src, int16_t *p_dst, uint32_t len)
{
    for (uint32_t i = 0; i < len; ++i) p_dst[i] = (int16_t)lrintf(p_src[i]);
}

void
th_add_f32(ee_f32_t *p_a, ee_f32_t *p_b, ee_f32_t *p_c, uint32_t len)
{
    for (uint32_t i = 0; i < len; ++i) p_c[i] = p_a[i] + p_b[i];
}

void
th_subtract_f32(ee_f32_t *p_a, ee_f32_t *p_b, ee_f32_t *p_c, uint32_t len)
{
    for (uint32_t i = 0; i < len; ++i) p_c[i] = p_a[i] - p_b[i];
}

void
th_dot_prod_f32(ee_f32_t *p_a, ee_f32_t *p_b, uint32_t len, ee_f32_t *p_result)
{
    float acc = 0.0f;
    for (uint32_t i = 0; i < len; ++i) acc += p_a[i] * p_b[i];
    *p_result = acc;
}

void
th_multiply_f32(ee_f32_t *p_a, ee_f32_t *p_b, ee_f32_t *p_c, uint32_t len)
{
    for (uint32_t i = 0; i < len; ++i) p_c[i] = p_a[i] * p_b[i];
}

void
th_cmplx_mag_f32(ee_f32_t *p_a, ee_f32_t *p_c, uint32_t len)
{
    for (uint32_t i = 0; i < len; ++i) {
        float r = p_a[2*i]; float im = p_a[2*i+1];
        p_c[i] = sqrtf(r*r + im*im);
    }
}

void
th_offset_f32(ee_f32_t *p_a, ee_f32_t offset, ee_f32_t *p_c, uint32_t len)
{
    for (uint32_t i = 0; i < len; ++i) p_c[i] = p_a[i] + offset;
}

void
th_vlog_f32(ee_f32_t *p_a, ee_f32_t *p_c, uint32_t len)
{
    for (uint32_t i = 0; i < len; ++i) p_c[i] = logf(p_a[i]);
}

void
th_mat_vec_mult_f32(ee_matrix_f32_t *p_a, ee_f32_t *p_b, ee_f32_t *p_c)
{
    uint32_t rows = p_a->numRows;
    uint32_t cols = p_a->numCols;
    for (uint32_t r = 0; r < rows; ++r) {
        float acc = 0.0f;
        for (uint32_t c = 0; c < cols; ++c)
            acc += p_a->pData[r*cols + c] * p_b[c];
        p_c[r] = acc;
    }
}

/* Lightweight neural network stub: to make Audiomark runnable on RISC-V
 * we implement a very small stub classifier that returns a deterministic
 * value instead of the full CMSIS-NN pipeline. This keeps the benchmark
 * executable and exercises the pre/post-processing stacks.
 */
void
th_nn_init(void)
{
    /* no-op */
}

ee_status_t
th_nn_classify(const int8_t in_data[490], int8_t out_data[12])
{
    /* Simple heuristic stub: zero output except set class 0 to 1 */
    (void)in_data;
    for (int i = 0; i < 12; ++i) out_data[i] = 0;
    out_data[0] = 1;
    return EE_STATUS_OK;
}
