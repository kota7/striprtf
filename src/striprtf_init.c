#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP striprtf_strip_helper(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP striprtf_to_hexstr(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"striprtf_strip_helper", (DL_FUNC) &striprtf_strip_helper, 5},
    {"striprtf_to_hexstr",    (DL_FUNC) &striprtf_to_hexstr,    2},
    {NULL, NULL, 0}
};

void R_init_striprtf(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
