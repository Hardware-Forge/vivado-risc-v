/* Minimal math implementations for bare-metal RISC-V builds.
 * These provide the small set of functions used by SpeexDSP/Audiomark
 * without linking libm.
 */

/* Constants */
#define M_PI_VAL   3.14159265358979323846
#define M_PI_2_VAL 1.57079632679489661923

/* Reduce x to [-pi, pi] range */
static double reduce_angle(double x)
{
    /* Reduce to [-2pi, 2pi] */
    double two_pi = 2.0 * M_PI_VAL;
    x = x - two_pi * (int)(x / two_pi);
    if (x > M_PI_VAL) x -= two_pi;
    if (x < -M_PI_VAL) x += two_pi;
    return x;
}

/* Sine approximation using Taylor series - good enough for FFT twiddles */
double sin(double x)
{
    x = reduce_angle(x);
    /* Taylor series: sin(x) = x - x^3/6 + x^5/120 - x^7/5040 + ... */
    double x2 = x * x;
    double x3 = x2 * x;
    double x5 = x3 * x2;
    double x7 = x5 * x2;
    double x9 = x7 * x2;
    double x11 = x9 * x2;
    return x - x3/6.0 + x5/120.0 - x7/5040.0 + x9/362880.0 - x11/39916800.0;
}

double cos(double x)
{
    x = reduce_angle(x);
    /* Taylor series: cos(x) = 1 - x^2/2 + x^4/24 - x^6/720 + ... */
    double x2 = x * x;
    double x4 = x2 * x2;
    double x6 = x4 * x2;
    double x8 = x6 * x2;
    double x10 = x8 * x2;
    return 1.0 - x2/2.0 + x4/24.0 - x6/720.0 + x8/40320.0 - x10/3628800.0;
}

double sqrt(double x)
{
    if (x <= 0.0) return 0.0;
    /* Newton-Raphson */
    double guess = x;
    for (int i = 0; i < 20; ++i) {
        guess = 0.5 * (guess + x / guess);
    }
    return guess;
}

double exp(double x)
{
    /* exp(x) = 1 + x + x^2/2! + x^3/3! + ... */
    /* Handle large values with range reduction */
    if (x > 700.0) return 1e308;
    if (x < -700.0) return 0.0;
    
    /* Range reduction: exp(x) = exp(k*ln2) * exp(r) where r < ln2 */
    double ln2 = 0.6931471805599453;
    int k = (int)(x / ln2);
    double r = x - k * ln2;
    
    /* Taylor for exp(r) */
    double result = 1.0;
    double term = 1.0;
    for (int i = 1; i < 20; ++i) {
        term *= r / i;
        result += term;
    }
    
    /* Multiply by 2^k */
    while (k > 0) { result *= 2.0; k--; }
    while (k < 0) { result *= 0.5; k++; }
    return result;
}

double log(double x)
{
    if (x <= 0.0) return -1e308;
    
    /* Range reduction to [1, 2): x = m * 2^e */
    int e = 0;
    while (x >= 2.0) { x *= 0.5; e++; }
    while (x < 1.0) { x *= 2.0; e--; }
    
    /* Now 1 <= x < 2; use log(x) = log(1 + (x-1)) */
    double y = x - 1.0;
    /* Taylor: log(1+y) = y - y^2/2 + y^3/3 - y^4/4 + ... */
    double result = 0.0;
    double term = y;
    for (int i = 1; i < 30; ++i) {
        result += term / i;
        term *= -y;
    }
    return result + e * 0.6931471805599453; /* + e * ln(2) */
}

double pow(double a, double b)
{
    if (a == 0.0) return 0.0;
    if (b == 0.0) return 1.0;
    /* pow(a,b) = exp(b * log(a)) */
    return exp(b * log(a));
}

double floor(double x)
{
    long i = (long)x;
    return (x < 0 && x != (double)i) ? i - 1 : i;
}

double fabs(double x)
{
    return (x < 0) ? -x : x;
}

/* Float versions - just cast through double for simplicity */
float sinf(float x)     { return (float)sin((double)x); }
float cosf(float x)     { return (float)cos((double)x); }
float sqrtf(float x)    { return (float)sqrt((double)x); }
float expf(float x)     { return (float)exp((double)x); }
float logf(float x)     { return (float)log((double)x); }
float powf(float a, float b) { return (float)pow((double)a, (double)b); }
float floorf(float x)   { return (float)floor((double)x); }
float fabsf(float x)    { return (x < 0) ? -x : x; }

long lrintf(float x)
{
    return (x >= 0) ? (long)(x + 0.5f) : (long)(x - 0.5f);
}

double atan(double x)
{
    /* atan approximation using polynomial */
    int neg = 0, inv = 0;
    if (x < 0) { x = -x; neg = 1; }
    if (x > 1.0) { x = 1.0 / x; inv = 1; }
    
    /* Polynomial approx for atan(x) for |x| <= 1 */
    double x2 = x * x;
    double result = x * (1.0 - x2 * (1.0/3.0 - x2 * (1.0/5.0 - x2 * (1.0/7.0 - x2 / 9.0))));
    
    if (inv) result = M_PI_2_VAL - result;
    if (neg) result = -result;
    return result;
}

float atanf(float x) { return (float)atan((double)x); }

double atan2(double y, double x)
{
    if (x > 0) return atan(y / x);
    if (x < 0) {
        if (y >= 0) return atan(y / x) + M_PI_VAL;
        return atan(y / x) - M_PI_VAL;
    }
    /* x == 0 */
    if (y > 0) return M_PI_2_VAL;
    if (y < 0) return -M_PI_2_VAL;
    return 0.0;
}

float atan2f(float y, float x) { return (float)atan2((double)y, (double)x); }
