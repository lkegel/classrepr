#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double d_ed(NumericVector x, NumericVector y, int TT) {
  double d = 0;

  for (int t = 0; t < TT; t++) {
    d += pow(x[t] - y[t], 2);
  }

  return(sqrt(d));
}
