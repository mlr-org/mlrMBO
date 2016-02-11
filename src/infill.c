#include <math.h>

#include <R.h>
#include <R_ext/Utils.h>
#include <Rinternals.h>

#include "macros.h"
#include "hv.h"

#define INF 1.0e14

SEXP c_sms_indicator(SEXP s_cbs, SEXP s_front, SEXP s_front2, SEXP s_eps, SEXP s_ref) {
  UNPACK_REAL_MATRIX_2(s_cbs, cbs, nrow_cbs);
  UNPACK_REAL_MATRIX(s_front, front, nrow_front, ncol_front);
  UNPACK_REAL_MATRIX(s_front2, front2, nrow_front2, ncol_front2);
  double* eps = REAL(s_eps);
  double* ref = REAL(s_ref);
  SEXP s_ret = PROTECT(allocVector(REALSXP, nrow_cbs));
  double* ret = REAL(s_ret);

  size_t i, j, k;
  double penalty_front_point, penalty, d, hvtot;
  int real_greater;

  /* calculate penalties for all cbs */
  for (i=0; i < nrow_cbs; i++) { /* loop cb points */
    penalty = 0; /* TW diss: formula (7.2) */
    for (j=0; j < nrow_front; j++) { /* loop front points */
      penalty_front_point = 1; /* set to 1 (neutral element) for later multiply */
      real_greater = 0;
      for (k=0; k < ncol_front; k++) { /* loop all dimensions */
        d = cbs[i + k * nrow_cbs] - front[j + k * nrow_front];
        if (d < -eps[k]) {
          /* violation! front-point does not eps-dominate cb-point, so no penalty for it*/
          penalty_front_point = 0;
          break;
        }
        if (d > -eps[k])
          real_greater = 1;
        penalty_front_point = penalty_front_point * (1 + MAX(d, 0));
      }
      if (penalty_front_point != 0 && real_greater) /* we had eps-dominance (hence penalty), add -1 */
        /* we want to min thge penalty later, getting close to 0 is good */
        penalty_front_point = -1 + penalty_front_point;
      else
        penalty_front_point = 0;
      if (penalty_front_point > penalty) /* get max of penalties for all front points */
        penalty = penalty_front_point;
    }
    ret[i] = penalty; /* set return value */
  }

  /* calculate HV contribution to front for all cb points */
  hvtot = fpli_hv(front2, nrow_front2, ncol_front2 - 1, ref); /* HV of front, exclude last 0 col */
  for (i=0; i < nrow_cbs; i++) { /* loop cb points */
    /* if no penalty for i-cb, front epsdom cb(i) was false, we then calculate HV contrib. */
    if (ret[i] == 0) {
      for (k=0; k < ncol_front; k++) { /* loop all dimensions */
        /* add cb point to last COLUMN of front2 (it is transposed! must be for fpli_hv) */
        front2[k + (ncol_front2 - 1) * nrow_front2] = cbs[i + k * nrow_cbs];
      }
      /* big HV contrib is nice, but we minize later */
      ret[i] = -(fpli_hv(front2, nrow_front2, ncol_front2, ref) - hvtot);
    }
  }

  UNPROTECT(1); /* s_ret */
  return(s_ret);
}


SEXP c_eps_indicator(SEXP s_cbs, SEXP s_front) {
  UNPACK_REAL_MATRIX_2(s_cbs, cbs, nrow_cbs);
  UNPACK_REAL_MATRIX(s_front, front, nrow_front, ncol_front);
  SEXP s_ret = PROTECT(allocVector(REALSXP, nrow_cbs));
  double* ret = REAL(s_ret);

  size_t i, j, k;
  double dist_2_points, dist_to_set, d;

  for (i=0; i < nrow_cbs; i++) { /* loop cb points */
    dist_to_set = INF;
    for (j=0; j < nrow_front; j++) { /* loop front points */
      /* get dist i-cb to j-front */
      dist_2_points = -INF;
      for (k=0; k < ncol_front; k++) { /* calculate (signed) L_inf dist (max norm) */
        d = front[j + k * nrow_front] - cbs[i + k * nrow_cbs];
        if (d > dist_2_points)
          dist_2_points = d;
      }
      if (dist_2_points < dist_to_set) /* found a closer point? set it */
        dist_to_set = dist_2_points;
    }
    /* set dist to set as crit val for i-cb point, but this distance should be
     * maximized, so -1 as we minize later */
    ret[i] = -dist_to_set;

  }
  UNPROTECT(1); /* s_ret */
  return(s_ret);
}


