#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int sgn0(double x) {
  return (0 <= x) - (x < 0);
}

// [[Rcpp::export]]
IntegerMatrix red_rld(NumericVector x) {
  int T = x.size();
  IntegerVector count(T + 1, 0);
  int sg = sgn0(x[0]);
  int start = 0;
  int rl;

  // Result
  int rls = 0;
  IntegerVector rl_array(T, 0);
  for (int t = 1; t < T; t++) {
    if (sgn0(x[t]) != sg) {
      rl = t - start;
      if (count[rl] == 0) {
        rl_array[rls] = rl;
        rls++;
      }
      count[rl]++;
      start = t;
      sg = -1 * sg;
    }
  }
  // Last segment
  rl = T - start;
  if (count[rl] == 0) {
    rl_array[rls] = rl;
    rls++;
  }
  count[rl]++;

  std::sort(rl_array.begin(), rl_array.begin() + rls);

  IntegerMatrix result(2, rls);
  for (int i = 0; i < rls; i++) {
    result(0, i) = rl_array[i];
    result(1, i) = count[rl_array[i]];
  }

  return(result);
}
