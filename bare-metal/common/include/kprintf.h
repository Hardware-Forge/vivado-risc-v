#ifndef _BOOT_KPRINTF_H
#define _BOOT_KPRINTF_H

#include <stdarg.h>

extern void kputc(char ch);
extern void kputs(const char *);
extern int kprintf(const char *, ...);
extern int vkprintf(const char *, va_list);

#endif /* _BOOT_KPRINTF_H */
