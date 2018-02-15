#include <R.h>
#include <Rmath.h>

void calc_weierstrass(double *x, int *nreal, double *res)
{
    int i, j;
    double sum;
    double a, b;
    int k_max;
    a = 0.5;
    b = 3.0;
    k_max = 20;
    *res = 0.0;
    for (i = 0; i < *nreal; i++) {
        sum = 0.0;
        for (j = 0; j <= k_max; j++) {
            sum += pow(a, j) * cos(2.0 * PI * pow(b, j) * (x[i] + 0.5));
        }
        *res += sum;
    }
}

void calc_schwefel(double *x, int *nreal, double *res)
{
    int i, j;
    double sum1, sum2;
    sum1 = 0.0;
    for (i = 0; i < *nreal; i++) {
        sum2 = 0.0;
        for (j = 0; j <= i; j++) {
            sum2 += x[j];
        }
        sum1 += sum2 * sum2;
    }
    *res = sum1;
}
