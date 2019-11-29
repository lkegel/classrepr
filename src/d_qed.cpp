#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double d_qed(NumericVector x, NumericVector y, int TT, double bsf) {
  double d = 0;

  for (int t = 0; t < TT && d < bsf; t++) {
    d += pow(x[t] - y[t], 2);
  }

  return(d);
}
