#include "common.h"

.section .text.start, "ax", @progbits
.globl _start
_start:
  li sp, BOOT_MEM_END

  # these extra functions initialize the baremetal SoC
  # call init
  # call welcome_message

  li a0, 0
  li a1, 0
  call main
_hang:
  wfi
  j _hang
