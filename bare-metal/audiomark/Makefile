OPTIMIZATION = -O3

# Source files
SRCS = head.S \
       main.c \
       th_api.c \
       audiomark/src/ee_audiomark.c \
       audiomark/src/ee_aec_f32.c \
       audiomark/src/ee_anr_f32.c \
       audiomark/src/ee_abf_f32.c \
       audiomark/src/ee_abf_f32_tables.c \
       audiomark/src/ee_kws.c \
       audiomark/src/ee_mfcc_f32.c \
       audiomark/src/ee_mfcc_f32_tables.c \
       audiomark/src/ee_nn_tables.c \
       audiomark/lib/speexdsp/libspeexdsp/buffer.c \
       audiomark/lib/speexdsp/libspeexdsp/fftwrap.c \
       audiomark/lib/speexdsp/libspeexdsp/filterbank.c \
       audiomark/lib/speexdsp/libspeexdsp/jitter.c \
       audiomark/lib/speexdsp/libspeexdsp/kiss_fft.c \
       audiomark/lib/speexdsp/libspeexdsp/kiss_fftr.c \
       audiomark/lib/speexdsp/libspeexdsp/mdf.c \
       audiomark/lib/speexdsp/libspeexdsp/preprocess.c \
       audiomark/lib/speexdsp/libspeexdsp/resample.c \
       audiomark/lib/speexdsp/libspeexdsp/scal.c \
       audiomark/lib/speexdsp/libspeexdsp/smallft.c

include ../common.mk

# Add include directories
CCFLAGS += -Iaudiomark/src \
           -Iaudiomark/lib/speexdsp/libspeexdsp \
           -Iaudiomark/lib/speexdsp/include \
           -Iaudiomark/lib/speexdsp/include/speex \
           -I.

# Defines
CCFLAGS += -DFLOATING_POINT -DEXPORT= -DOS_SUPPORT_CUSTOM -D__RESTRICT= -DUSE_KISS_FFT -DRELEASE

