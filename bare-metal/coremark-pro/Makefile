# Unified CoreMark-PRO Makefile for Bare-Metal FPGA

# Toolchain
RISCV_GCC = ../../workspace/gcc/riscv/bin/riscv64-unknown-elf-gcc
RISCV_OBJDUMP = ../../workspace/gcc/riscv/bin/riscv64-unknown-elf-objdump
CC = $(RISCV_GCC)

# Directories
MITH = mith
BENCHMARKS = benchmarks
WORKLOADS = workloads
COMMON = ../common

# Common Flags
CFLAGS += -march=rv64imfd -mabi=lp64d -fno-builtin -ffreestanding -mcmodel=medany -O2 -g -Wall
CFLAGS += -Wno-unused-variable -Wno-implicit-function-declaration
CFLAGS += -DHOST_EXAMPLE_CODE=1 -DUSE_CLOCK=0 -DNO_ALIGNED_ALLOC=1 -DHAVE_FILEIO=0 -DFAKE_FILEIO=1
CFLAGS += -DEE_SIZEOF_LONG=8 -DEE_SIZEOF_PTR=8 -DEE_PTR_ALIGN=8 -DUSE_SINGLE_CONTEXT=1
CFLAGS += -DFPGA_CPU_CLK_FREQ=50 -Dprintf=kprintf -DUSE_MATH_H=0 -DAL_ISFINITE=1
CFLAGS += -I$(MITH)/include -I$(MITH)/al/include -I$(COMMON)/include -I.
CFLAGS += -fno-pic -fno-common

# MITH Sources
MITH_SRCS = $(wildcard $(MITH)/src/*.c) $(wildcard $(MITH)/al/src/*.c)
# Exclude al_smp.c as we use al_single.c logic (though both might be present, al_single usually sufficient or al_smp verified previously)
# Actually, al_single.c is what we want for single core. al_file.c stems from HOST.
MITH_SRCS := $(filter-out $(MITH)/al/src/al_smp_pthread.c, $(MITH_SRCS))

# --- Workload Definitions ---

# 1. CORE
CORE_SRCS = $(wildcard $(BENCHMARKS)/core/*.c) $(WORKLOADS)/core/core.c
CORE_FLAGS = -Dmain=run_core -I$(BENCHMARKS)/core

# 2. CJPEG
CJPEG_SRCS = $(wildcard $(BENCHMARKS)/consumer_v2/cjpeg/*.c) $(WORKLOADS)/cjpeg-rose7-preset/cjpeg-rose7-preset.c $(BENCHMARKS)/consumer_v2/cjpeg/data/Rose256_bmp.c
CJPEG_FLAGS = -Dmain=run_cjpeg -DSELECT_PRESET_ID=1 -DUSE_PRESET -I$(BENCHMARKS)/consumer_v2/cjpeg -I$(BENCHMARKS)/consumer_v2/cjpeg/data

# 3. LINEAR_ALG (FP32)
LIN_SRCS = $(wildcard $(BENCHMARKS)/fp/linpack/*.c) $(WORKLOADS)/linear_alg-mid-100x100-sp/linear_alg-mid-100x100-sp.c $(BENCHMARKS)/fp/linpack/ref/inputs_f32.c
LIN_FLAGS = -Dmain=run_linear -DUSE_FP32=1 -I$(BENCHMARKS)/fp/linpack

# Symbol renaming to avoid collisions for init_preset_X
LOOPS_RENAMES = -Dinit_preset_0=loops_ip_0 -Dinit_preset_1=loops_ip_1 -Dinit_preset_2=loops_ip_2 -Dinit_preset_3=loops_ip_3 -Dinit_preset_4=loops_ip_4 -Dinit_preset_5=loops_ip_5 -Dinit_preset_6=loops_ip_6 -Dinit_preset_7=loops_ip_7 -Dinit_preset_8=loops_ip_8 -Dinit_preset_9=loops_ip_9
NNET_RENAMES  = -Dinit_preset_0=nnet_ip_0 -Dinit_preset_1=nnet_ip_1 -Dinit_preset_2=nnet_ip_2 -Dinit_preset_3=nnet_ip_3 -Dinit_preset_4=nnet_ip_4 -Dinit_preset_5=nnet_ip_5 -Dinit_preset_6=nnet_ip_6 -Dinit_preset_7=nnet_ip_7 -Dinit_preset_8=nnet_ip_8 -Dinit_preset_9=nnet_ip_9
RADIX_RENAMES = -Dinit_preset_0=radix_ip_0 -Dinit_preset_1=radix_ip_1 -Dinit_preset_2=radix_ip_2 -Dinit_preset_3=radix_ip_3 -Dinit_preset_4=radix_ip_4 -Dinit_preset_5=radix_ip_5 -Dinit_preset_6=radix_ip_6 -Dinit_preset_7=radix_ip_7 -Dinit_preset_8=radix_ip_8 -Dinit_preset_9=radix_ip_9

# 4. LOOPS (FP32)
LOOPS_SRCS = $(wildcard $(BENCHMARKS)/fp/loops/*.c) $(WORKLOADS)/loops-all-mid-10k-sp/loops-all-mid-10k-sp.c $(wildcard $(BENCHMARKS)/fp/loops/ref-sp/*.c)
LOOPS_FLAGS = -Dmain=run_loops -DUSE_FP32=1 -I$(BENCHMARKS)/fp/loops $(LOOPS_RENAMES)

# 5. NNET (FP64)
NNET_SRCS = $(wildcard $(BENCHMARKS)/fp/nnet/*.c) $(WORKLOADS)/nnet_test/nnet_test.c $(wildcard $(BENCHMARKS)/fp/nnet/ref/*.c)
NNET_FLAGS = -Dmain=run_nnet -DUSE_FP64=1 -I$(BENCHMARKS)/fp/nnet $(NNET_RENAMES)

# 6. PARSER
PARSER_SRCS = $(wildcard $(BENCHMARKS)/darkmark/parser/*.c) $(WORKLOADS)/parser-125k/parser-125k.c
PARSER_FLAGS = -Dmain=run_parser -I$(BENCHMARKS)/darkmark/parser

# 7. RADIX2 (FP64)
RADIX_SRCS = $(wildcard $(BENCHMARKS)/fp/fft_radix2/*.c) $(WORKLOADS)/radix2-big-64k/radix2-big-64k.c $(wildcard $(BENCHMARKS)/fp/fft_radix2/ref/*.c)
RADIX_FLAGS = -Dmain=run_radix2 -DUSE_FP64=1 -I$(BENCHMARKS)/fp/fft_radix2 $(RADIX_RENAMES)

# 8. SHA
SHA_SRCS = $(wildcard $(BENCHMARKS)/darkmark/sha/*.c) $(WORKLOADS)/sha-test/sha-test.c
SHA_FLAGS = -Dmain=run_sha -I$(BENCHMARKS)/darkmark/sha

# 9. ZIP
ZIP_SRCS = $(wildcard $(BENCHMARKS)/darkmark/zip/*.c) $(wildcard $(BENCHMARKS)/darkmark/zip/zlib-1.2.8/*.c) $(WORKLOADS)/zip-test/zip-test.c
# Exclude gzread/gzwrite as per readme
ZIP_SRCS := $(filter-out $(BENCHMARKS)/darkmark/zip/zlib-1.2.8/gzread.c $(BENCHMARKS)/darkmark/zip/zlib-1.2.8/gzwrite.c, $(ZIP_SRCS))
ZIP_FLAGS = -Dmain=run_zip -DMITH_MEMORY_ONLY_VERSION -DZLIB_COMPAT_ALL -DZLIB_ANSI -I$(BENCHMARKS)/darkmark/zip -I$(BENCHMARKS)/darkmark/zip/zlib-1.2.8

# Wrapper
WRAPPER_SRCS = $(WORKLOADS)/all/all.c
WRAPPER_FLAGS = 

# Stubs & Common
STUB_SRCS = stubs.c $(wildcard $(COMMON)/src/*.c)
STUB_FLAGS = -I$(COMMON)/include

# Combined Sources and Objects
ALL_SRCS = $(STUB_SRCS) $(MITH_SRCS) $(CORE_SRCS) $(CJPEG_SRCS) $(LIN_SRCS) $(LOOPS_SRCS) $(NNET_SRCS) $(PARSER_SRCS) $(RADIX_SRCS) $(SHA_SRCS) $(ZIP_SRCS) $(WRAPPER_SRCS)
OBJS = $(ALL_SRCS:.c=.o)

# Object Lists
MITH_OBJS = $(MITH_SRCS:.c=.o)
CORE_OBJS = $(CORE_SRCS:.c=.o)
CJPEG_OBJS = $(CJPEG_SRCS:.c=.o)
LIN_OBJS = $(LIN_SRCS:.c=.o)
LOOPS_OBJS = $(LOOPS_SRCS:.c=.o)
NNET_OBJS = $(NNET_SRCS:.c=.o)
PARSER_OBJS = $(PARSER_SRCS:.c=.o)
RADIX_OBJS = $(RADIX_SRCS:.c=.o)
SHA_OBJS = $(SHA_SRCS:.c=.o)
ZIP_OBJS = $(ZIP_SRCS:.c=.o)
WRAPPER_OBJS = $(WRAPPER_SRCS:.c=.o)
STUB_OBJS = $(STUB_SRCS:.c=.o)

ALL_OBJS = $(MITH_OBJS) $(CORE_OBJS) $(CJPEG_OBJS) $(LIN_OBJS) $(LOOPS_OBJS) $(NNET_OBJS) $(PARSER_OBJS) $(RADIX_OBJS) $(SHA_OBJS) $(ZIP_OBJS) $(WRAPPER_OBJS) $(STUB_OBJS)

# Target Rules
.PHONY: all clean

all: boot.elf

# Custom Compile Rules to apply specific flags
$(CORE_OBJS): %.o: %.c
	$(CC) $(CFLAGS) $(CORE_FLAGS) -c $< -o $@

$(CJPEG_OBJS): %.o: %.c
	$(CC) $(CFLAGS) $(CJPEG_FLAGS) -c $< -o $@

$(LIN_OBJS): %.o: %.c
	$(CC) $(CFLAGS) $(LIN_FLAGS) -c $< -o $@

$(LOOPS_OBJS): %.o: %.c
	$(CC) $(CFLAGS) $(LOOPS_FLAGS) -c $< -o $@

$(NNET_OBJS): %.o: %.c
	$(CC) $(CFLAGS) $(NNET_FLAGS) -c $< -o $@

$(PARSER_OBJS): %.o: %.c
	$(CC) $(CFLAGS) $(PARSER_FLAGS) -c $< -o $@

$(RADIX_OBJS): %.o: %.c
	$(CC) $(CFLAGS) $(RADIX_FLAGS) -c $< -o $@

$(SHA_OBJS): %.o: %.c
	$(CC) $(CFLAGS) $(SHA_FLAGS) -c $< -o $@

$(ZIP_OBJS): %.o: %.c
	$(CC) $(CFLAGS) $(ZIP_FLAGS) -c $< -o $@

$(WRAPPER_OBJS): %.o: %.c
	$(CC) $(CFLAGS) $(WRAPPER_FLAGS) -c $< -o $@

# Default compile for shared files (MITH, Stubs)
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@
# Handle .S files
%.o: %.S
	$(CC) $(CFLAGS) -c $< -o $@

boot.elf: $(ALL_OBJS) head.S
	$(CC) $(CFLAGS) -o $@ head.S $(ALL_OBJS) -static -nostartfiles -T $(COMMON)/src/main.lds -Wl,-Map=boot.map
	$(RISCV_OBJDUMP) -h -p $@

clean:
	rm -f $(ALL_OBJS) boot.elf
