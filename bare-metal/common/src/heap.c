#include <stddef.h>
#include <stdint.h>

// These are declared weakly in newlib, but we'll override them entirely
extern void* _sbrk(ptrdiff_t increment);

void* malloc(size_t size) {
    void *ptr = _sbrk(size);
    return (ptr == (void*)-1) ? NULL : ptr;
}

void free(void *ptr) {
    // No-op: simple bump allocator can't free
}

void *calloc(size_t nmemb, size_t size) {
    size_t total = nmemb * size;
    void *p = malloc(total);
    if (p) {
        unsigned char *b = (unsigned char *)p;
        for (size_t i = 0; i < total; ++i) b[i] = 0;
    }
    return p;
}

void *realloc(void *ptr, size_t size) {
    if (!ptr) return malloc(size);
    // bump allocator cannot actually shrink/expand; allocate new block
    void *p = malloc(size);
    if (!p) return NULL;
    // We can't know original size; best-effort copy of size bytes
    unsigned char *d = (unsigned char *)p;
    unsigned char *s = (unsigned char *)ptr;
    for (size_t i = 0; i < size; ++i) d[i] = s[i];
    return p;
}

extern char _end; // defined by linker
static char *heap_end;
extern char _ram_end; /* defined by linker script as end of RAM region */

void* _sbrk(ptrdiff_t incr) {
    if (!heap_end)
        heap_end = &_end;

    /* Bump allocator: do not support negative increments */
    if (incr < 0) {
        return (void *)-1;
    }

    /* Ensure returned pointer is 16-byte aligned for SIMD/FFT buffers */
    uintptr_t cur = (uintptr_t)heap_end;
    uintptr_t aligned = (cur + 15) & ~(uintptr_t)15;
    char *prev_heap_end = (char *)aligned;

    /* Check we don't grow past RAM end provided by linker script */
    uintptr_t new_end = (uintptr_t)aligned + (uintptr_t)incr;
    uintptr_t ram_end = (uintptr_t)&_ram_end;
    if (new_end > ram_end) {
        return (void *)-1; /* indicate OOM */
    }

    heap_end = (char *)new_end;
    return (void *)prev_heap_end;
}
