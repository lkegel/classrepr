// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// d_ed
double d_ed(NumericVector x, NumericVector y, int TT);
RcppExport SEXP _classrepr_d_ed(SEXP xSEXP, SEXP ySEXP, SEXP TTSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type TT(TTSEXP);
    rcpp_result_gen = Rcpp::wrap(d_ed(x, y, TT));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_classrepr_d_ed", (DL_FUNC) &_classrepr_d_ed, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_classrepr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
