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
  Progress p(max_time, display_progress);

  // ========== Simple Event Weighted Variance ==========
  double cum_sum = 0.0;  // running sum over t = 1..j

  for (int j = 0; j < max_time; j++) {

    // Term for t = j (0-based), added to the running total
    double factor = 1.0 / ((1.0 - uj[j]) * (1.0 - uj[j]));

    // Quadratic form: sum_m p_m W_m^2 - (sum_m p_m W_m)^2
    double sum_pW2 = 0.0;
    double sum_pW  = 0.0;
    for (int m = 0; m < k; m++) {
      double pw = pj(j, m) * W[m];
      sum_pW  += pw;
      sum_pW2 += pw * W[m];
    }
    double quad_form = sum_pW2 - sum_pW * sum_pW;

    cum_sum += factor * quad_form / n_riskset[j];

    // final variance term
    total_var[j] = s_sq[j] * cum_sum;

    p.increment();
  }

  return total_var;
}

