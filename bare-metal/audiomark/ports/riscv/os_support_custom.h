/* RISC-V 64-bit compatible os_support_custom.h for speexdsp.
 * This replaces the TI version which uses 32-bit pointer casts.
 */
#ifndef OS_SUPPORT_CUSTOM_H
#define OS_SUPPORT_CUSTOM_H

#include <stddef.h>
#include <stdint.h>
#include <string.h>

/* Global heap pointers used by the AEC/ANR per-component heaps */
extern char *spxGlobalHeapPtr;
extern char *spxGlobalHeapEnd;
extern char *spxGlobalScratchPtr;
extern char *spxGlobalScratchEnd;

/* Alignment mask - use 16-byte alignment for safety */
#define BLOCK_MASK 15

#define OVERRIDE_SPEEX_ALLOC
static inline void *speex_alloc(int size)
{
    char *ptr;
    
    /* Align pointer */
    ptr = (char *)(((uintptr_t)spxGlobalHeapPtr + BLOCK_MASK) & ~(uintptr_t)BLOCK_MASK);
    
    /* Update heap pointer */
    spxGlobalHeapPtr = ptr + size;
    
    if (spxGlobalHeapPtr > spxGlobalHeapEnd) {
        return 0;
    }
    
    /* Clear memory (speex_alloc must return zeroed memory) */
    memset(ptr, 0, size);
    return ptr;
}

#define OVERRIDE_SPEEX_ALLOC_SCRATCH
static inline void *speex_alloc_scratch(int size)
{
    char *ptr;
    
    ptr = (char *)(((uintptr_t)spxGlobalScratchPtr + BLOCK_MASK) & ~(uintptr_t)BLOCK_MASK);
    spxGlobalScratchPtr = ptr + size;
    
    if (spxGlobalScratchPtr > spxGlobalScratchEnd) {
        return 0;
    }
    
    memset(ptr, 0, size);
    return ptr;
}

#define OVERRIDE_SPEEX_REALLOC
static inline void *speex_realloc(void *ptr, int size)
{
    (void)ptr;
    (void)size;
    return 0; /* Not supported */
}

#define OVERRIDE_SPEEX_FREE
static inline void speex_free(void *ptr)
{
    (void)ptr; /* No-op for bump allocator */
}

#define OVERRIDE_SPEEX_FREE_SCRATCH
static inline void speex_free_scratch(void *ptr)
{
    (void)ptr;
}

#endif /* OS_SUPPORT_CUSTOM_H */
