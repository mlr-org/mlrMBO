#include <math.h>

#include <R.h>
#include <R_ext/Utils.h>
#include <Rinternals.h>

#include "macros.h"
#include "hv.h"

#define INF 1.0e14

SEXP c_sms_indicator(SEXP s_lcbs, SEXP s_front, SEXP s_front2, SEXP s_eps, SEXP s_ref) {
  UNPACK_REAL_MATRIX_2(s_lcbs, lcbs, nrow_lcbs);
  UNPACK_REAL_MATRIX(s_front, front, nrow_front, ncol_front);
  UNPACK_REAL_MATRIX(s_front2, front2, nrow_front2, ncol_front2);
  double eps = asReal(s_eps);
  double* ref = REAL(s_ref);
  SEXP s_ret = PROTECT(allocVector(REALSXP, nrow_lcbs));
  double* ret = REAL(s_ret);

  size_t i, j, k;
  double penalty_front_point, penalty, d, hvtot;

  /* calculate HV contribution to front for all lcb points */
  hvtot = fpli_hv(front2, nrow_front2, ncol_front2 - 1, ref); /* HV of front, exclude last 0 col */
  for (i=0; i < nrow_lcbs; i++) { /* iterate all lcb points */
    for (k=0; k < ncol_front; k++) { /* loop all dimensions */
      /* add lcb point to last COLUMN of front2 (it is transposed! must be for fpli_hv) */
      front2[k + (ncol_front2 - 1) * nrow_front2] = lcbs[i + k * nrow_lcbs];
    }
    ret[i] = -(fpli_hv(front2, nrow_front2, ncol_front2, ref) - hvtot);
  }

  /* calculate penalties for all lcbs */
  for (i=0; i < nrow_lcbs; i++) { /* loop lcb points */
    penalty = 0;
    for (j=0; j < nrow_front; j++) { /* loop front points */
      penalty_front_point = 1; /* set to 1 (neutral element) for later multiply */
      for (k=0; k < ncol_front; k++) { /* loop all dimensions */
        d = lcbs[i + k * nrow_lcbs] - front[j + k * nrow_front];
        if (d < -eps) {
          /* violation! front-point does not eps-dominate lcb point, so no penalty for it*/
          penalty_front_point = 0;
          break;
        } else {
          penalty_front_point = penalty_front_point * (1 + MAX(d, 0));
        }
      }
      if (penalty_front_point != 0) /* we had a penalty, add -1 */
        penalty_front_point = -1 + penalty_front_point;
      if (penalty_front_point > penalty) /* get max of penalties for all front points */
        penalty = penalty_front_point;
    }
    ret[i] = ret[i] + penalty; /* add to HV return value from above */
  }

  UNPROTECT(1); /* s_ret */
  return(s_ret);
}


SEXP c_eps_indicator(SEXP s_lcbs, SEXP s_front) {
  UNPACK_REAL_MATRIX_2(s_lcbs, lcbs, nrow_lcbs);
  UNPACK_REAL_MATRIX(s_front, front, nrow_front, ncol_front);
  SEXP s_ret = PROTECT(allocVector(REALSXP, nrow_lcbs));
  double* ret = REAL(s_ret);

  size_t i, j, k;
  double dist_2_points, dist_to_set, d;

  for (i=0; i < nrow_lcbs; i++) { /* loop lcb points */
    dist_to_set = INF;
    for (j=0; j < nrow_front; j++) { /* loop front points */
      /* get dist i-lcb to j-front */
      dist_2_points = -INF;
      for (k=0; k < ncol_front; k++) { /* calculate (signed) L_inf dist (max norm) */
        d = front[j + k * nrow_front] - lcbs[i + k * nrow_lcbs];
        if (d > dist_2_points)
          dist_2_points = d;
      }
      if (dist_2_points < dist_to_set) /* found a closer point? set it */
        dist_to_set = dist_2_points;
    }
    ret[i] = dist_to_set; /* set dist to set as crit val for i-lcb point */
  }
  UNPROTECT(1); /* s_ret */
  return(s_ret);
}



