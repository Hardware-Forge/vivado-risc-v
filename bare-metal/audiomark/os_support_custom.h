#ifndef OS_SUPPORT_CUSTOM_H
#define OS_SUPPORT_CUSTOM_H

#define OVERRIDE_SPEEX_WARNING
static inline void speex_warning(const char *str) {}

#define OVERRIDE_SPEEX_WARNING_INT
static inline void speex_warning_int(const char *str, int val) {}

#define OVERRIDE_SPEEX_NOTIFY
static inline void speex_notify(const char *str) {}

#define OVERRIDE_SPEEX_FATAL
static inline void _speex_fatal(const char *str, const char *file, int line)
{
   while(1);
}

#define OVERRIDE_SPEEX_ALLOC
extern void *my_speex_alloc(int size);
static inline void *speex_alloc(int size) { return my_speex_alloc(size); }

#define OVERRIDE_SPEEX_REALLOC
extern void *my_speex_realloc(void *ptr, int size);
static inline void *speex_realloc(void *ptr, int size) { return my_speex_realloc(ptr, size); }

#define OVERRIDE_SPEEX_FREE
extern void my_speex_free(void *ptr);
static inline void speex_free(void *ptr) { my_speex_free(ptr); }

#define OVERRIDE_SPEEX_ALLOC_SCRATCH
static inline void *speex_alloc_scratch(int size) { return my_speex_alloc(size); }

#define OVERRIDE_SPEEX_FREE_SCRATCH
static inline void speex_free_scratch(void *ptr) { my_speex_free(ptr); }

#endif
