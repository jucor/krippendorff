

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
#include <cmath>
#include <cstdio>
using namespace Rcpp;

// Given the original set of pairs for a unit, generate nboot alphas
// @param deviations List of deviations for each possible pair
// @param nboot Number of bootstrap samples to take
// @return a NumericVector of nboot summed deviations for that unit. Summing over all units and substracting to 1 will give alpha.
// [[Rcpp::export]]
NumericVector bootstrap_within_unit(NumericVector deviations, int nboot) {

  int n = deviations.size();

  // Only one pair: no need to resample a dirac
  if (n == 1)
    return NumericVector(nboot, deviations[0]);

  int i, j;
  NumericVector summed_deviations(nboot, 0.0);
  for (i = 0; i < nboot; i++) {
    NumericVector sampled_deviations = RcppArmadillo::sample(deviations, n, true, NumericVector::create());

    // Special condition for index 1: ensure that it differs from index 1, by manually these two it.
    int first = int(floor(runif(1, 0, n)[0]));
    int second;
    do {
       second = int(floor(runif(1, 0, n)[0]));
    } while (second == first);
    sampled_deviations[0] = deviations[first];
    sampled_deviations[1] = deviations[second];
    for (j = 0; j < n; j++)
      summed_deviations[i] += sampled_deviations[j];
  }

  return summed_deviations;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
bootstrap_within_unit(c(.1, .2, .3), 5)
bootstrap_within_unit(c(.1), 5)
*/
