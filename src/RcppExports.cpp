// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// countNominal
double countNominal(NumericVector n);
RcppExport SEXP krippendorff_countNominal(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(countNominal(n));
    return rcpp_result_gen;
END_RCPP
}
