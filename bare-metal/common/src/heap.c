#include <stddef.h>
#include <stdio.h>
#include <stdarg.h>
#include "kprintf.h"

extern char _end; // defined by linker
static char *heap_end;

extern int printf(const char*, ...);

void* _sbrk(ptrdiff_t incr) {
    if (!heap_end)
        heap_end = &_end;

    char *prev_heap_end = heap_end;
    heap_end += incr;
    return (void *)prev_heap_end;
}

typedef struct FreeBlock {
    size_t size;
    struct FreeBlock *next;
} FreeBlock;

static FreeBlock *free_list = NULL;

void *memset(void *s, int c, size_t n) {
    unsigned char *p = (unsigned char *)s;
    while (n--) {
        *p++ = (unsigned char)c;
    }
    return s;
}

void *memcpy(void *dest, const void *src, size_t n) {
   char *d = (char *)dest;
   const char *s = (const char *)src;
   while (n--) {
       *d++ = *s++;
   }
   return dest;
}

void *malloc(size_t size) {
    if (size == 0) size = 1;
    // Align size to 8 bytes and ensure minimum size for FreeBlock linkage
    size = (size + 7) & ~7;
    if (size < sizeof(FreeBlock*)) size = sizeof(FreeBlock*); // Ensure space for 'next'

    FreeBlock **prev = &free_list;
    FreeBlock *curr = free_list;

    while (curr) {
        if (curr->size >= size) {
            *prev = curr->next;
            return (char*)curr + sizeof(size_t);
        }
        prev = &curr->next;
        curr = curr->next;
    }

    size_t alloc_size = size + sizeof(size_t);
    void *ptr = _sbrk(alloc_size);
    if (ptr == (void*)-1) return NULL;
    *(size_t*)ptr = size;
    return (char*)ptr + sizeof(size_t);
}

void free(void *ptr) {
    if (!ptr) return;
    FreeBlock *block = (FreeBlock*)((char*)ptr - sizeof(size_t));
    block->next = free_list;
    free_list = block;
}

void *calloc(size_t nmemb, size_t size) {
    size_t total = nmemb * size;
    void *ptr = malloc(total);
    if (ptr) {
        memset(ptr, 0, total);
    }
    return ptr;
}

void *realloc(void *ptr, size_t size) {
    if (!ptr) return malloc(size);
    if (size == 0) {
        free(ptr);
        return NULL;
    }
    
    void *new_ptr = malloc(size);
    if (!new_ptr) return NULL;
    
    size_t old_size = *(((size_t*)ptr) - 1);
    size_t copy_size = (old_size < size) ? old_size : size;
    memcpy(new_ptr, ptr, copy_size);
    
    free(ptr);
    return new_ptr;
}


void exit(int status) {
    while (1);
}

int fprintf(FILE *stream, const char *format, ...) {
    va_list vl;
    va_start(vl, format);
    int ret = vkprintf(format, vl);
    va_end(vl);
    return ret;
}

static unsigned long rand_next = 1;

int rand(void) {
    rand_next = rand_next * 1103515245 + 12345;
    return (unsigned int)(rand_next/65536) % 32768;
}

void srand(unsigned int seed) {
    rand_next = seed;
}
