/*
(C) 2014 EEMBC(R).  All rights reserved.                            

All EEMBC Benchmark Software are products of EEMBC 
and are provided under the terms of the EEMBC Benchmark License Agreements.  
The EEMBC Benchmark Software are proprietary intellectual properties of EEMBC and its Members 
and is protected under all applicable laws, including all applicable copyright laws.  
If you received this EEMBC Benchmark Software without having 
a currently effective EEMBC Benchmark License Agreement, you must discontinue use. 
Please refer to LICENSE.md for the specific license agreement that pertains to this Benchmark Software.
*/

#include "th_math.h"

/* Undefine macros from th_math.h to allow custom implementation */
#undef th_sin
#undef th_cos
#undef th_tan
#undef th_pow
#undef th_exp
#undef th_ln
#undef th_log10
#undef th_sqrt
#undef th_fabs
#undef th_floor
#undef th_tanh
#undef th_atan
#undef th_pow_64
#undef th_log10_f64

#define IMPL_MATH(S, T, V0, V1, V2, PI_V, LN2_V, LN10_INV) \
T th_fabs_##S(T x) { \
    return (x < V0) ? -x : x; \
} \
T th_floor_##S(T x) { \
    return (T)((long)x); \
} \
T th_sqrt_##S(T x) { \
   if (x <= V0) return V0; \
   T guess = x / V2; \
   T prev; \
   int i; \
   for(i=0; i<20; i++) { \
       prev = guess; \
       guess = (guess + x/guess) / V2; \
       if(th_fabs_##S(guess - prev) < (T)1e-10) break; \
   } \
   return guess; \
} \
T th_exp_##S(T x) { \
    if (x < 0) return V1 / th_exp_##S(-x); \
    if (x > 1.0) { \
        T e = th_exp_##S(x / 2.0); \
        return e * e; \
    } \
    T sum = V1; \
    T term = V1; \
    int i; \
    for (i = 1; i < 25; i++) { \
        term *= x / i; \
        sum += term; \
        if (th_fabs_##S(term) < (T)1e-12) break; \
    } \
    return sum; \
} \
T th_ln_##S(T x) { \
    if (x <= V0) return -V1 / V0; /* -inf: -1/0 */ \
    /* Robust range reduction */ \
    T mx = x; \
    int k = 0; \
    int loop_limit = 100000; \
    while (mx > V2 && loop_limit-- > 0) { mx /= V2; k++; } \
    while (mx < (V1/V2) && loop_limit-- > 0) { mx *= V2; k--; } \
    if (loop_limit <= 0) return V0; /* Fail safe */ \
    T y = (mx - V1) / (mx + V1); \
    T y2 = y * y; \
    T num = y; \
    T den = V1; \
    T sum = V0; \
    int i; \
    for (i = 0; i < 20; i++) { \
        sum += num / den; \
        num *= y2; \
        den += V2; \
    } \
    return V2 * sum + k * LN2_V; \
} \
T th_pow_##S(T base, T exponent) { \
    if (exponent == (int)exponent) { \
        int e = (int)exponent; \
        T res = V1; \
        T b = base; \
        if (e < 0) { b = V1/b; e = -e; } \
        while (e > 0) { \
            if (e % 2 == 1) res *= b; \
            b *= b; \
            e /= 2; \
        } \
        return res; \
    } \
    if (base <= V0) return V0; \
    return th_exp_##S(exponent * th_ln_##S(base)); \
} \
T th_log10_##S(T x) { \
    return th_ln_##S(x) * LN10_INV; \
} \
T th_sin_##S(T x) { \
    /* Simple robustness check for large inputs/Inf */ \
    if (x > 1e9 || x < -1e9) return V0; /* Avoid hang on large/Inf */ \
    while (x > PI_V) x -= V2*PI_V; \
    while (x < -PI_V) x += V2*PI_V; \
    T sum = x; \
    T term = x; \
    T x2 = x * x; \
    int i; \
    for (i = 1; i < 15; i++) { \
        term *= -x2 / ((2*i)*(2*i+1)); \
        sum += term; \
    } \
    return sum; \
} \
T th_cos_##S(T x) { \
    if (x > 1e9 || x < -1e9) return V0; \
    while (x > PI_V) x -= V2*PI_V; \
    while (x < -PI_V) x += V2*PI_V; \
    T sum = V1; \
    T term = V1; \
    T x2 = x * x; \
    int i; \
    for (i = 1; i < 15; i++) { \
        term *= -x2 / ((2*i-1)*(2*i)); \
        sum += term; \
    } \
    return sum; \
} \
T th_tan_##S(T x) { return th_sin_##S(x) / th_cos_##S(x); } \
T th_tanh_##S(T x) { \
    T px = th_exp_##S(x); \
    T nx = th_exp_##S(-x); \
    return (px - nx) / (px + nx); \
} \
T th_atan_##S(T x) { \
    int inv = 0; \
    int sign = (x < V0) ? -1 : 1; \
    x = th_fabs_##S(x); \
    if (x > V1) { x = V1/x; inv = 1; } \
    T sum = x; \
    T term = x; \
    T x2 = x * x; \
    int i; \
    for (i = 1; i < 50; i++) { \
        term *= -x2; \
        sum += term / (2*i+1); \
    } \
    if (inv) sum = (PI_V/V2) - sum; \
    return sign * sum; \
} \
T th_cosh_##S(T x) { return V0; } \
T th_sinh_##S(T x) { return V0; }

IMPL_MATH(f64, double, 0.0, 1.0, 2.0, 3.1415926535897932, 0.69314718056, 0.4342944819)
IMPL_MATH(f32, float, 0.0f, 1.0f, 2.0f, 3.1415927f, 0.6931472f, 0.4342945f)
