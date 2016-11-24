#ifndef MLRMBO_MACROS_H_
#define MLRMBO_MACROS_H_

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#define MAX(a,b) (((a)>(b))?(a):(b))

#define UNPACK_REAL_VECTOR(S, D, N) \
    double *D = REAL(S); \
    const R_len_t N = length(S);

#define UNPACK_REAL_MATRIX(S, D, N, K) \
    double *D = REAL(S); \
    const R_len_t N = nrows(S); \
    const R_len_t K = ncols(S);

#define UNPACK_REAL_MATRIX_2(S, D, N) \
    double *D = REAL(S); \
    const R_len_t N = nrows(S);

#endif
