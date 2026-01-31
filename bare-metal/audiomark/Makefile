OPTIMIZATION = -O2
INCLUDE_DIR = .

# Add include paths needed by the speexdsp sources and the RISC-V port
# Do NOT define OS_SUPPORT_CUSTOM - use the default calloc/free which we provide
TEST_SPECIFIC_FLAGS = -Iports/riscv -Ilib/speexdsp -Ilib/speexdsp/include -Ilib/speexdsp/include/speex -Ilib/speexdsp/libspeexdsp -Isrc -DFLOATING_POINT -DEXPORT= -DUSE_KISS_FFT=1

# Minimal set of sources pulled from the CMake Audiomark list to allow
# building a bare-metal firmware. This is intentionally a single-threaded
# build using the "barebones" th_api implementation.
SRCS = head.S \
       ports/riscv/main_bare.c \
       src/ee_audiomark.c \
       src/ee_aec_f32.c \
       src/ee_anr_f32.c \
       src/ee_abf_f32.c \
       src/ee_abf_f32_tables.c \
       src/ee_kws.c \
       src/ee_mfcc_f32.c \
       src/ee_mfcc_f32_tables.c \
       src/ee_nn_tables.c \
       lib/speexdsp/libspeexdsp/buffer.c \
       lib/speexdsp/libspeexdsp/filterbank.c \
       lib/speexdsp/libspeexdsp/jitter.c \
       lib/speexdsp/libspeexdsp/kiss_fft.c \
       lib/speexdsp/libspeexdsp/kiss_fftr.c \
       lib/speexdsp/libspeexdsp/mdf.c \
       lib/speexdsp/libspeexdsp/preprocess.c \
       lib/speexdsp/libspeexdsp/resample.c \
       lib/speexdsp/libspeexdsp/scal.c \
       lib/speexdsp/libspeexdsp/smallft.c \
       ports/riscv/fft_adapter.c \
       ports/riscv/speex_wrappers.c \
       ports/riscv/math_stubs.c \
       ports/riscv/th_api.c

include ../common.mk
