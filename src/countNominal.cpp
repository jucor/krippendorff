#include <Rcpp.h>
using namespace Rcpp;

// TODO(jucor): check if long long are enough -- might have to go for double
// Compute n times the mismatch probability

// @param n
// [[Rcpp::export]]
double countNominal(NumericVector n) {
// TODO(jucor): might need to rewrite with better numerical precision...
    double ratio, cardinal;
    int d = n.size();
    int i, j;

    cardinal = 0;
    for (i=0; i<d; i++)
         cardinal += n[i];

    ratio = 0;
    for (i = 0; i < d; i++)
      for  (j = i+1; j < d; j++)
        ratio += n[i] * n[j] / (cardinal - 1);

    return ratio;
}
