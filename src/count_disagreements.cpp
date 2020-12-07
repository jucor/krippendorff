#include <Rcpp.h>
using namespace Rcpp;

// Compute disagreements
//
// More precisely, return N times the disagreement probability, where
// N is the total number of observations.
//
// @param counts Vector of counts per value
// [[Rcpp::export]]
double count_disagreements(NumericVector counts) {
// TODO(jucor): might need to rewrite with better numerical precision...
    double ratio, cardinal;
    int d = counts.size();
    int i, j;

    cardinal = sum(counts);

    ratio = 0;
    // Only compute upper-triangular disagreements to avoid duplicates
    // No need to compute the diagonal terms
    for (i = 0; i < d; i++)
      for (j = i+1; j < d; j++)
        ratio += counts[i] * counts[j] / (cardinal - 1);

    return ratio;
}
