// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// make_age_effect
NumericVector make_age_effect(List terms, IntegerVector dim, List mappings, NumericVector b, NumericMatrix X);
RcppExport SEXP _bage_make_age_effect(SEXP termsSEXP, SEXP dimSEXP, SEXP mappingsSEXP, SEXP bSEXP, SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type terms(termsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type dim(dimSEXP);
    Rcpp::traits::input_parameter< List >::type mappings(mappingsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(make_age_effect(terms, dim, mappings, b, X));
    return rcpp_result_gen;
END_RCPP
}
// make_linear_pred
NumericVector make_linear_pred(List terms, IntegerVector dim, List mappings);
RcppExport SEXP _bage_make_linear_pred(SEXP termsSEXP, SEXP dimSEXP, SEXP mappingsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type terms(termsSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type dim(dimSEXP);
    Rcpp::traits::input_parameter< List >::type mappings(mappingsSEXP);
    rcpp_result_gen = Rcpp::wrap(make_linear_pred(terms, dim, mappings));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_bage_make_age_effect", (DL_FUNC) &_bage_make_age_effect, 5},
    {"_bage_make_linear_pred", (DL_FUNC) &_bage_make_linear_pred, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_bage(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
