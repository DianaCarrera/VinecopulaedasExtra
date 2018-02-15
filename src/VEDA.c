#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

#include <dml.h>

#include "VEDA.h"

static void GSLRngFinalizer(SEXP Rng)
{
    gsl_rng *rng;

    if (R_ExternalPtrAddr(Rng)) {
        rng = R_ExternalPtrAddr(Rng);
        gsl_rng_free(rng);
        R_ClearExternalPtr(Rng);
    }
}

SEXP GSLRngAlloc()
{
    SEXP Rng;
    gsl_rng *rng;

    GetRNGstate();
    rng = gsl_rng_alloc(gsl_rng_taus);
    gsl_rng_set(rng, ftrunc(UINT_MAX * unif_rand()));
    PutRNGstate();
    PROTECT(Rng = R_MakeExternalPtr(rng, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(Rng, GSLRngFinalizer, TRUE);
    UNPROTECT(1);

    return Rng;
}

static void DMLVineFinalizer(SEXP Vine)
{
    dml_vine_t *vine;

    if (R_ExternalPtrAddr(Vine)) {
        vine = R_ExternalPtrAddr(Vine);
        dml_vine_free(vine);
        R_ClearExternalPtr(Vine);
    }
}

SEXP DMLVineFit(SEXP Data, SEXP VineType, SEXP VineWeight, SEXP VineTrunc,
                SEXP IndepTest, SEXP IndepTestLevel, SEXP CopulaTypes,
                SEXP CopulaSelect, SEXP Rng)
{
    SEXP Data_dim;
    gsl_matrix *data;
    size_t data_nrow, data_ncol;
    dml_vine_type_t vine_type;
    dml_vine_weight_t vine_weight;
    dml_vine_trunc_t vine_trunc;
    dml_copula_indeptest_t indeptest;
    double indeptest_level;
    dml_copula_type_t *copula_types;
    size_t copula_types_size;
    dml_copula_select_t copula_select;
    gsl_rng *rng;
    SEXP Vine;
    dml_vine_t *vine;

    // Data
    PROTECT(Data_dim = allocVector(INTSXP, 2));
    Data_dim = getAttrib(Data, R_DimSymbol);
    data_nrow = INTEGER(Data_dim)[0];
    data_ncol = INTEGER(Data_dim)[1];
    UNPROTECT(1);
    gsl_matrix_const_view data_trans
        = gsl_matrix_const_view_array(REAL(Data), data_ncol, data_nrow);
    data = gsl_matrix_alloc(data_nrow, data_ncol);
    gsl_matrix_transpose_memcpy(data, &data_trans.matrix);
    // VineType
    vine_type = (dml_vine_type_t) INTEGER(VineType)[0];
    // VineWeight
    vine_weight = (dml_vine_weight_t) INTEGER(VineWeight)[0];
    // VineTrunc
    vine_trunc = (dml_vine_trunc_t) INTEGER(VineTrunc)[0];
    // IndepTest
    indeptest = (dml_copula_indeptest_t) INTEGER(IndepTest)[0];
    // IndepTestLevel
    indeptest_level = REAL(IndepTestLevel)[0];
    // CopulaTypes
    copula_types_size = LENGTH(CopulaTypes);
    copula_types = (dml_copula_type_t *) R_alloc(copula_types_size, sizeof(dml_copula_type_t));
    for (size_t i = 0; i < copula_types_size; i++) {
        copula_types[i] = (dml_copula_type_t) INTEGER(CopulaTypes)[i];
    }
    // CopulaSelect
    copula_select = (dml_copula_select_t) INTEGER(CopulaSelect)[0];
    // Rng
    rng = R_ExternalPtrAddr(Rng);

    vine = dml_vine_alloc(vine_type, data_ncol);
    dml_vine_fit(vine, data, vine_weight, vine_trunc, indeptest, indeptest_level,
                 copula_types, copula_types_size, copula_select, rng);
    PROTECT(Vine = R_MakeExternalPtr(vine, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(Vine, DMLVineFinalizer, TRUE);
    UNPROTECT(1);

    gsl_matrix_free(data);

    return Vine;
}

SEXP DMLVineRan(SEXP Vine, SEXP Rng, SEXP PopSize)
{
    dml_vine_t *vine;
    gsl_rng *rng;
    size_t data_nrow, data_ncol;
    gsl_matrix *data;
    SEXP Data;

    // Vine
    vine = R_ExternalPtrAddr(Vine);
    // Rng
    rng = R_ExternalPtrAddr(Rng);

    data_nrow = INTEGER(PopSize)[0];
    data_ncol = vine->dim;
    data = gsl_matrix_alloc(data_nrow, data_ncol);
    dml_vine_ran(vine, rng, data);
    PROTECT(Data = allocMatrix(REALSXP, data_nrow, data_ncol));
    gsl_matrix_view data_trans
        = gsl_matrix_view_array(REAL(Data), data_ncol, data_nrow);
    gsl_matrix_transpose_memcpy(&data_trans.matrix, data);
    UNPROTECT(1);

    gsl_matrix_free(data);

    return Data;
}
