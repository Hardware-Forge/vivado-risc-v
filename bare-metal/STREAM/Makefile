################################################################################
# STREAM Makefile
#
# Supports two build modes:
#  - Host build (default): builds the original STREAM host binaries
#  - Bare-metal build: set BARE=1 when invoking make to build for RISC-V
#    Example: make BARE=1
################################################################################

BARE ?= 1

ifeq ($(BARE),1)

OPTIMIZATION = -O2
INCLUDE_DIR = .

# Choose a small default array size suitable for embedded runs (elements)
TEST_SPECIFIC_FLAGS = -DSTREAM_ARRAY_SIZE=1000000 -DNTIMES=10 -DBARE=1

SRCS = head.S stream.c mysecond_bare.c

include ../common.mk

else

CC = gcc
CFLAGS = -O2 -fopenmp

FC = gfortran
FFLAGS = -O2 -fopenmp

all: stream_f.exe stream_c.exe

stream_f.exe: stream.f mysecond.o
	$(CC) $(CFLAGS) -c mysecond.c
	$(FC) $(FFLAGS) -c stream.f
	$(FC) $(FFLAGS) stream.o mysecond.o -o stream_f.exe

stream_c.exe: stream.c
	$(CC) $(CFLAGS) stream.c -o stream_c.exe

clean:
	rm -f stream_f.exe stream_c.exe *.o

# an example of a more complex build line for the Intel icc compiler
stream.icc: stream.c
	icc -O3 -xCORE-AVX2 -ffreestanding -qopenmp -DSTREAM_ARRAY_SIZE=80000000 -DNTIMES=20 stream.c -o stream.omp.AVX2.80M.20x.icc

endif
