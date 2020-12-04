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

// Compute mutual disagreements
//
// Used for disagreement between coders and standards.
//
// This can be called on each unit, or on the whole dataset. In
// the latter case, it will be summing over all units.
//
// @param standards Vector of frequencies per value for the standard
// @param standards Vector of frequencies per value for the coders
// [[Rcpp::export]]
double mutual_disagreements(NumericVector standard,
                            NumericVector coders) {
// TODO(jucor): might need to rewrite with better numerical precision...
    double ss = sum(standard);
    double ratio = 0;
    int d = standard.size();

    for (int i = 0; i < d; i++)
      for (int j = 0; j < d; j++) {
        if (i == j) continue;
        ratio += coders[i] * standard[j] / ss;
      }

    return ratio;
}
