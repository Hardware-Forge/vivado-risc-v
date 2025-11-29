/**
 * Copyright (C) 2022 EEMBC
 * Copyright (C) 2022 Arm Limited
 *
 * All EEMBC Benchmark Software are products of EEMBC and are provided under the
 * terms of the EEMBC Benchmark License Agreements. The EEMBC Benchmark Software
 * are proprietary intellectual properties of EEMBC and its Members and is
 * protected under all applicable laws, including all applicable copyright laws.
 *
 * If you received this EEMBC Benchmark Software without having a currently
 * effective EEMBC Benchmark License Agreement, you must discontinue use.
 */

#include <stdint.h>
#include <stdbool.h>

#include "ee_audiomark.h"
#include "kprintf.h"
#include "fpu.h"
#include <stdlib.h>
#include <stdarg.h>

int printf(const char *fmt, ...);
int fprintf(void *stream, const char *fmt, ...);
int rand(void);
void srand(unsigned int seed);

void exit(int status) {
    while(1);
}

void abort(void) {
    while(1);
}

void init(void)
{
    // Initialize FPU
    enable_fpu();
}

extern void init_speex_heap(void);

void welcome_message(void)
{
    printf("AudioMark Benchmark Started\n");
}

static void vprintf_internal(const char *fmt, va_list vl) {
    int is_format = 0;
    int is_long = 0;
    int is_char = 0;
    char c;

    while ((c = *fmt++) != '\0') {
        if (is_format) {
            switch (c) {
            case 'l': is_long = 1; continue;
            case 'h': is_char = 1; continue;
            case 'x': {
                unsigned long n;
                long i;
                if (is_long) {
                    n = va_arg(vl, unsigned long);
                    i = (sizeof(unsigned long) << 3) - 4;
                } else {
                    n = va_arg(vl, unsigned int);
                    i = is_char ? 4 : (sizeof(unsigned int) << 3) - 4;
                }
                for (; i >= 0; i -= 4) {
                    long d = (n >> i) & 0xF;
                    kputc(d < 10 ? '0' + d : 'a' + d - 10);
                }
                break;
            }
            case 'd': {
                char buf[32];
                long n;
                long i = sizeof(buf);
                if (is_long) n = va_arg(vl, long);
                else n = va_arg(vl, int);
                if (n < 0) { kputc('-'); n = -n; }
                while (i > 0) {
                    buf[--i] = n % 10 + '0';
                    n = n / 10;
                    if (n == 0) break;
                }
                while (i < sizeof(buf)) kputc(buf[i++]);
                break;
            }
            case 's': {
                const char * s = va_arg(vl, const char *);
                while (*s) kputc(*s++);
                break;
            }
            case 'c':
                kputc(va_arg(vl, int));
                break;
            }
            is_format = 0;
            is_long = 0;
            is_char = 0;
        } else if (c == '%') {
            is_format = 1;
        } else if (c == '\n') {
            kputc('\r');
            kputc('\n');
        } else {
            kputc(c);
        }
    }
}

int printf(const char *fmt, ...) {
    va_list vl;
    va_start(vl, fmt);
    vprintf_internal(fmt, vl);
    va_end(vl);
    return 0;
}

int fprintf(void *stream, const char *fmt, ...) {
    va_list vl;
    va_start(vl, fmt);
    vprintf_internal(fmt, vl);
    va_end(vl);
    return 0;
}

int main(void)
{
    init();
    init_speex_heap();
    welcome_message();

    if (ee_audiomark_initialize() != EE_STATUS_OK)
    {
        printf("Initialization failed\n");
        return -1;
    }

    printf("Initialization successful\n");

    // Run the benchmark
    // The original main.c runs it for a certain duration or iterations.
    // ee_audiomark_run() processes one frame?
    // Let's check ee_audiomark.c
    
    // In the original main.c:
    /*
    while (1) {
        status = ee_audiomark_run();
        if (status != EE_STATUS_OK) break;
        // check time
    }
    */
    
    // We can run it for a fixed number of iterations for now.
    int iterations = 100; // Example
    for (int i = 0; i < iterations; i++) {
        if (ee_audiomark_run() != EE_STATUS_OK) {
            printf("Run failed at iteration %d\n", i);
            break;
        }
        if (i % 10 == 0) {
            printf(".");
        }
    }
    printf("\n");

    ee_audiomark_release();

    printf("AudioMark Benchmark Completed\n");

    return 0;
}

int rand(void) {
    static unsigned long next = 1;
    next = next * 1103515245 + 12345;
    return (unsigned int)(next/65536) % 32768;
}

void srand(unsigned int seed) {
    // Ignore
}
