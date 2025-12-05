# define FPGA CPU clock frequency in MHz
FPGA_CPU_CLK_FREQ = 100

CROSS_COMPILE = ../../workspace/gcc/riscv/bin/riscv64-unknown-elf-

CC=$(CROSS_COMPILE)gcc
OBJCOPY=$(CROSS_COMPILE)objcopy
OBJDUMP=$(CROSS_COMPILE)objdump

CFLAGS = -march=rv64imfd -mabi=lp64d -fno-builtin -ffreestanding -mcmodel=medany

COMPILER_VERSION := $(shell $(CC) --version | head -n 1)
COMPILER_FLAGS := $(CFLAGS) $(OPTIMIZATION) 

CCFLAGS = $(COMPILER_FLAGS) -Wall $(TEST_SPECIFIC_FLAGS)
CCFLAGS += -fno-pic -fno-common -g -I../common/include -I. -I$(INCLUDE_DIR)
CCFLAGS += '-DCOMPILER_VERSION="$(COMPILER_VERSION)"' '-DCOMPILER_FLAGS="$(COMPILER_FLAGS)"' '-DFPGA_CPU_CLK_FREQ=$(FPGA_CPU_CLK_FREQ)'

# Use the lightweight in-tree `kprintf` implementation instead of newlib's
# printf to avoid pulling in large/newlib symbols (e.g. _impure_ptr) which
# can cause relocation/linker issues for bare-metal builds.
CCFLAGS += -Dprintf=kprintf

LFLAGS = -static -nostartfiles -T ../common/src/main.lds

COMMON_SRCS := $(wildcard ../common/src/*.c)
SRCS += $(COMMON_SRCS)

all: boot.elf

boot.elf: $(SRCS) 
	make -C ../.. workspace/gcc/riscv
	$(CC) $(CCFLAGS) -o $@ $(SRCS) $(LFLAGS)
	$(OBJDUMP) -h -p $@

clean:
	rm -f *.elf
