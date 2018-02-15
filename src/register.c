#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include "VEDA.h"

R_CallMethodDef callMethods[] = {
    { "GSLRngAlloc", (DL_FUNC) &GSLRngAlloc, 0 },
    { "DMLVineFit", (DL_FUNC) &DMLVineFit, 9 },
    { "DMLVineRan", (DL_FUNC) &DMLVineRan, 3 },
    { NULL, NULL, 0 }
};

void R_init_copulaedasExtra(DllInfo *info) {
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
