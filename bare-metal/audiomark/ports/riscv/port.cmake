include_directories(${PORT_DIR})

# Minimal RISC-V port: use the barebones th_api implementation which uses
# standard libc functions and the project's malloc/free wrappers. This avoids
# pulling in ARM-specific CMSIS-DSP/CMSIS-NN sources.

# RISC-V port: provide a full-ish implementation based on libspeexdsp's
# kiss FFT routines and simple C fallbacks for DSP/NN helpers.
include_directories(${PORT_DIR})
include_directories(${PORT_DIR}/..)

set(PORT_SOURCE
    ${PORT_DIR}/th_api.c
)

message(STATUS "Audiomark: using RISC-V port (ports/riscv)")
