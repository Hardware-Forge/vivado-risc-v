
#include <math.h>

double log10(double x) { return 0.0; }
double pow(double x, double y) { return 0.0; }
double sin(double x) { return 0.0; }
double cos(double x) { return 0.0; }
double sqrt(double x) { return 0.0; }
double ceil(double x) { return 0.0; }
double fabs(double x) { return 0.0; }
double floor(double x) { return 0.0; }

#include <stdlib.h>
#include <string.h>
char *strdup(const char *s) {
    size_t len = strlen(s) + 1;
    char *new = malloc(len);
    if (new == NULL) return NULL;
    return (char *)memcpy(new, s, len);
}

#include <limits.h>

/* ctype stubs */
/* ctype stubs */
/* #include <ctype.h> removed to avoid redef */

int isspace(int c) {
    return (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v');
}
int isdigit(int c) {
    return (c >= '0' && c <= '9');
}
int isalpha(int c) {
    return ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'));
}
int isupper(int c) {
    return (c >= 'A' && c <= 'Z');
}

long strtol(const char *nptr, char **endptr, int base) {
    const char *s = nptr;
    unsigned long acc;
    int c;
    unsigned long cutoff;
    int neg = 0, any, cutlim;

    do {
        c = *s++;
    } while (isspace(c));
    if (c == '-') {
        neg = 1;
        c = *s++;
    } else if (c == '+')
        c = *s++;
    if ((base == 0 || base == 16) &&
        c == '0' && (*s == 'x' || *s == 'X')) {
        c = s[1];
        s += 2;
        base = 16;
    }
    if (base == 0)
        base = c == '0' ? 8 : 10;

    cutoff = neg ? -(unsigned long)LONG_MIN: LONG_MAX;
    cutlim = cutoff % (unsigned long)base;
    cutoff /= (unsigned long)base;
    for (acc = 0, any = 0;; c = *s++) {
        if (isdigit(c))
            c -= '0';
        else if (isalpha(c))
            c -= isupper(c) ? 'A' - 10 : 'a' - 10;
        else
            break;
        if (c >= base)
            break;
        if (any < 0 || acc > cutoff || (acc == cutoff && c > cutlim))
            any = -1;
        else {
            any = 1;
            acc *= base;
            acc += c;
        }
    }
    if (any < 0) {
        acc = neg ? LONG_MIN : LONG_MAX;
    } else if (neg)
        acc = -acc;
    if (endptr != 0)
        *endptr = (char *) (any ? s - 1 : nptr);
    return (acc);
}

long _strtol_r(struct _reent *reent, const char *nptr, char **endptr, int base) {
    return strtol(nptr, endptr, base);
}

#include <stdarg.h>
#include "kprintf.h"

int vprintf(const char *fmt, va_list ap) {
    return vkprintf(fmt, ap);
}

/* Helper for vsnprintf */
static void _mini_itoa(long value, unsigned int radix, int uppercase, int unsig,
	 char *buffer, int zero_pad) {
	char *pbuffer = buffer;
	int negative = 0;
	int i, len;

	/* No support for negative numbers with non-10 radix */
	if (radix == 10 && !unsig && value < 0) {
		negative = 1;
		value = -value;
	}

	/* Conversion */
	do {
		int digit = value % radix;
		*(pbuffer++) = (digit < 10 ? '0' + digit : (uppercase ? 'A' : 'a') + digit - 10);
		value /= radix;
	} while (value > 0);

	if (negative)
		*(pbuffer++) = '-';

	if (zero_pad > 0) {
		int used = pbuffer - buffer;
		while (used < zero_pad) {
			*(pbuffer++) = '0';
			used++;
		}
	}
	*pbuffer = '\0';

	/* Reverse string */
	len = pbuffer - buffer;
	for (i = 0; i < len / 2; i++) {
		char j = buffer[i];
		buffer[i] = buffer[len - 1 - i];
		buffer[len - 1 - i] = j;
	}
}

int vsnprintf(char *str, size_t size, const char *format, va_list ap) {
	size_t count = 0;
	char c;
	char *buf_ptr = str;
	const char *fmt = format;

	if (!str || size == 0) return 0;

	while ((c = *fmt++) != 0 && count < size - 1) {
		if (c != '%') {
			*buf_ptr++ = c;
			count++;
			continue;
		}

		c = *fmt++;
		if (c == '0') { // Check for zero padding (simplified)
             // TODO: Real padding support. For now just eat it?
             // gen_parse_buf uses %06d.
             // Let's assume %06d format only.
             if (*fmt >= '0' && *fmt <= '9') {
                 // Hack: support %06d specifically?
             }
        }
        
        // Simple support for %d, %s, %u, %x
        // Note: gen_parse_buf uses %s and %d.
        // It also uses %06d in debug prints.
        int width = 0;
        int zero_pad = 0;
        if (c == '0') {
             zero_pad = 1;
             c = *fmt++;
        }
        while (c >= '0' && c <= '9') {
             width = width * 10 + (c - '0');
             c = *fmt++;
        }

		switch (c) {
		case 'd': {
			long val = va_arg(ap, long); // assuming long for simplicity, or int promoted
			char tmp[32];
            int pad = (zero_pad && width > 0) ? width : 0;
			_mini_itoa(val, 10, 0, 0, tmp, pad);
			char *t = tmp;
			while (*t && count < size - 1) {
				*buf_ptr++ = *t++;
				count++;
			}
			break;
		}
		case 'u': {
			unsigned long val = va_arg(ap, unsigned long);
			char tmp[32];
            int pad = (zero_pad && width > 0) ? width : 0;
			_mini_itoa(val, 10, 0, 1, tmp, pad);
			char *t = tmp;
			while (*t && count < size - 1) {
				*buf_ptr++ = *t++;
				count++;
			}
			break;
		}
		case 'x': 
        case 'p': {
			unsigned long val = va_arg(ap, unsigned long);
			char tmp[32];
			_mini_itoa(val, 16, 0, 1, tmp, 0);
			char *t = tmp;
			while (*t && count < size - 1) {
				*buf_ptr++ = *t++;
				count++;
			}
			break;
		}
		case 's': {
			char *val = va_arg(ap, char *);
            if (!val) val = "(null)";
			while (*val && count < size - 1) {
				*buf_ptr++ = *val++;
				count++;
			}
			break;
		}
        case 'c': {
            int val = va_arg(ap, int);
            *buf_ptr++ = (char)val;
            count++;
            break;
        }
		default:
			*buf_ptr++ = c;
			count++;
			break;
		}
	}



	*buf_ptr = '\0';
    /* th_printf("vsnprintf: fmt='%s' -> '%s' (%d)\n", format, str, count); */
	return count;
}

int vsprintf(char *str, const char *format, va_list ap) {
    /* th_printf("vsprintf called: '%s'\n", format); */
    return vsnprintf(str, 4096, format, ap); // Sufficiently large buffer
}

/* Use system definition of struct lconv and _reent */
#include <sys/reent.h>

struct lconv *localeconv(void) {
    return NULL;
}

struct lconv *_localeconv_r(struct _reent *r) {
    return NULL;
}

/* Stubs to avoid linking incompatible locale.o from libc */
#include <locale.h>
char *setlocale(int category, const char *locale) {
    return "C";
}

char *_setlocale_r(struct _reent *r, int category, const char *locale) {
    return "C";
}

/* Locale stubs */
/* #include <locale.h> removed - reusing sys/reent.h include or earlier locale.h */
/* localeconv and _localeconv_r already defined above */

/* Global locale stubs */
extern const char _ctype_[]; /* Forward declaration */
long __global_locale[32] = {0}; /* Dummy storage for __global_locale */

struct lconv *__localeconv_l(void *l) {
    return NULL;
}

const char *__locale_ctype_ptr(void) {
    return _ctype_;
}
const char *__locale_ctype_ptr_l(void *l) { /* void* to match stub signature */
    return _ctype_;
}
int __locale_mb_cur_max(void) {
    return 1;
}

/* Newlib ctype flags */
#define	_U	01
#define	_L	02
#define	_N	04
#define	_S	010
#define _P	020
#define _C	040
#define _X	0100
#define	_B	0200

/* Populated _ctype_ array */
const char _ctype_[257] = {
    0, /* EOF */
    /* 0-31 Control chars */
    _C, _C, _C, _C, _C, _C, _C, _C,
    _C, _S|_B|_C, _S|_C, _S|_C, _S|_C, _S|_C, _C, _C, /* \t \n \v \f \r */
    _C, _C, _C, _C, _C, _C, _C, _C,
    _C, _C, _C, _C, _C, _C, _C, _C,
    /* 32-63 */
    _S|_B, _P, _P, _P, _P, _P, _P, _P, /* Space ! " # $ % & ' */
    _P, _P, _P, _P, _P, _P, _P, _P, /* ( ) * + , - . / */
    _N|_X, _N|_X, _N|_X, _N|_X, _N|_X, _N|_X, _N|_X, _N|_X, /* 0-7 */
    _N|_X, _N|_X, _P, _P, _P, _P, _P, _P, /* 8 9 : ; < = > ? */
    /* 64-95 */
    _P, _U|_X, _U|_X, _U|_X, _U|_X, _U|_X, _U|_X, _U, /* @ A-G */
    _U, _U, _U, _U, _U, _U, _U, _U, /* H-O */
    _U, _U, _U, _U, _U, _U, _U, _U, /* P-W */
    _U, _U, _U, _P, _P, _P, _P, _P, /* X Y Z [ \ ] ^ _ */
    /* 96-127 */
    _P, _L|_X, _L|_X, _L|_X, _L|_X, _L|_X, _L|_X, _L, /* ` a-g */
    _L, _L, _L, _L, _L, _L, _L, _L, /* h-o */
    _L, _L, _L, _L, _L, _L, _L, _L, /* p-w */
    _L, _L, _L, _P, _P, _P, _P, _C, /* x y z { | } ~ DEL */
    /* 128-255 (0) */
    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0
};
