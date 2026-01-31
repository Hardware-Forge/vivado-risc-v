#!/bin/bash
set -e

# Config
CROSS_COMPILE=/tools/riscv/bin/riscv64-unknown-elf-
CC=${CROSS_COMPILE}gcc
OBJDUMP=${CROSS_COMPILE}objdump
OBJCOPY=${CROSS_COMPILE}objcopy

# Flags (matching standard coremark build)
CFLAGS="-static -fno-common -fno-builtin -ffreestanding -mno-relax -march=rv64imfd -mabi=lp64d -mcmodel=medany"
LFLAGS="-T ../common/src/main.lds -L. -nostartfiles -static"

# Compile head.S
$CC $CFLAGS -c head.S -I../common/include -o head.o

# Compile common sources
$CC $CFLAGS -c ../common/src/kprintf.c -I../common/include -o kprintf.o
$CC $CFLAGS -c ../common/src/heap.c -I../common/include -o heap.o
$CC $CFLAGS -c ../common/src/perf_counters.c -I../common/include -o perf_counters.o
$CC $CFLAGS -c ../common/src/fpu.c -I../common/include -o fpu.o

# Core workload objects
CORE_OBJS="\
builds/riscv64/riscv-gcc64/obj/workloads/core/core.o \
builds/riscv64/riscv-gcc64/obj/bench/core/core_mith.o \
builds/riscv64/riscv-gcc64/obj/bench/core/core_util.o \
builds/riscv64/riscv-gcc64/obj/bench/core/core_matrix.o \
builds/riscv64/riscv-gcc64/obj/bench/core/core_state.o \
builds/riscv64/riscv-gcc64/obj/bench/core/core_portme.o \
builds/riscv64/riscv-gcc64/obj/bench/core/core_list_join.o \
builds/riscv64/riscv-gcc64/obj/mith.a"

# Link
echo "Linking boot.elf..."
$CC $CFLAGS -o boot.elf \
    head.o kprintf.o heap.o perf_counters.o fpu.o \
    $CORE_OBJS \
    $LFLAGS -lm

echo "Creating disassembly..."
$OBJDUMP -D boot.elf > boot.dump

echo "Done! boot.elf created."
