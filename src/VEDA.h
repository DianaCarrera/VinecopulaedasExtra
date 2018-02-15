SEXP GSLRngAlloc();

SEXP DMLVineFit(SEXP Data, SEXP VineType, SEXP VineWeight, SEXP VineTrunc,
                SEXP IndepTest, SEXP IndepTestLevel, SEXP CopulaTypes,
                SEXP CopulaSelect, SEXP Rng);

SEXP DMLVineRan(SEXP Vine, SEXP Rng, SEXP PopSize);

