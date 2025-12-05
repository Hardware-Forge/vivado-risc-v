/* Minimal newlib compatibility symbols for bare-metal linking.
 * Provide `exit` and `_impure_ptr`/_global_impure_ptr to avoid pulling
 * newlib objects that reference reentrancy data structures which cause
 * relocation issues on our bare-metal link.
 */

#include <stdint.h>

/* Provide a minimal exit implementation */
void exit(int status)
{
    (void)status;
    /* simple infinite loop to halt */
    for (;;)
        ;
}

void _exit(int status) __attribute__((alias("exit")));

/* Provide dummy reentrancy pointers so objects referencing them link.
 * Use plain void* to avoid pulling newlib headers.
 */
void* _impure_ptr = 0;
void* _global_impure_ptr = 0;
