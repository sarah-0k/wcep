#include <Rcpp.h>
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
using namespace Rcpp;

// [[Rcpp::export]]

NumericVector varvec_simple(NumericVector W,
                            NumericVector uj,
                            NumericMatrix pj,
                            NumericVector s_sq,
                            IntegerVector n_riskset,
                            int max_time,
                            bool display_progress = true) {

  int k = W.size();
  NumericVector total_var(max_time);

  //variance progress bar
  Progress p(k + max_time, display_progress);

  // ========== Simple Event Weighted Variance ==========
  const double* w   = W.begin();
  const double* P   = pj.begin();     // column-major: P[j + m * nrow]
  const int     nr  = pj.nrow();

  // Column-major-friendly pass: accumulate per-time sums, one column of pj at a time
  std::vector<double> sum_pW(max_time, 0.0), sum_pW2(max_time, 0.0);
  for (int m = 0; m < k; m++) {
    const double* col = P + (size_t)m * nr;
    const double wm = w[m], wm2 = wm * wm;
    for (int j = 0; j < max_time; j++) {
      sum_pW[j]  += col[j] * wm;
      sum_pW2[j] += col[j] * wm2;
    }
    p.increment();
  }

  double cum_sum = 0.0;
  for (int j = 0; j < max_time; j++) {
    const double d = 1.0 - uj[j];
    const double quad_form = sum_pW2[j] - sum_pW[j] * sum_pW[j];
    cum_sum += quad_form / (d * d * n_riskset[j]);
    total_var[j] = s_sq[j] * cum_sum;

    p.increment();
  }

  return total_var;
}

