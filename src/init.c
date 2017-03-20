#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP c_eps_indicator(SEXP, SEXP);
extern SEXP c_sms_indicator(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"c_eps_indicator", (DL_FUNC) &c_eps_indicator, 2},
    {"c_sms_indicator", (DL_FUNC) &c_sms_indicator, 5},
    {NULL, NULL, 0}
};

void R_init_mlrMBO(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
