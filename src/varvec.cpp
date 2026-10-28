#include <Rcpp.h>
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
using namespace Rcpp;

// [[Rcpp::export]]

NumericVector varvec(NumericVector W,
                     NumericVector uj,
                     NumericMatrix pj,
                     NumericVector s_sq,
                     IntegerVector censor_times,
                     IntegerVector n_riskset,
                     int n,
                     int max_time,
                     bool display_progress = true) {

  int k = W.size();
  Progress p(max_time*2 + n, display_progress);

  // ---- Step 1: per-time quantities a_t, b_t and their cumulative sums ----
  // A[m] = sum_{t=1..m} a_t,  B[m] = sum_{t=1..m} b_t  (A[0] = B[0] = 0)
  std::vector<double> A(max_time + 1, 0.0), B(max_time + 1, 0.0);

  for (int t = 0; t < max_time; t++) {
    double sum_pW = 0.0, sum_pW2 = 0.0;
    for (int m = 0; m < k; m++) {
      double pw = pj(t, m) * W[m];
      sum_pW  += pw;
      sum_pW2 += pw * W[m];
    }
    double quad_form = sum_pW2 - sum_pW * sum_pW;   // W'(diag(p) - pp')W
    double d = 1.0 - uj[t];
    double a_t = quad_form / (d * d);

    A[t + 1] = A[t] + a_t;
    B[t + 1] = B[t] + a_t / n_riskset[t];
    p.increment();
  }

  // ---- Step 2: bucket subjects by censor time (one pass over n) ----
  // cnt[c]  = number of subjects with censor time c
  // wsum[c] = sum of (n - i) over those subjects (0-based i, as in the original)
  std::vector<double> cnt(max_time + 1, 0.0), wsum(max_time + 1, 0.0);
  for (int i = 0; i < n; i++) {
    int c = censor_times[i];
    if (c >= 1 && c <= max_time) {
      cnt[c]  += 1.0;
      wsum[c] += (double)(n - i);
    }
    p.increment();
  }

  // ---- Step 3: running totals over subjects with c_i <= j ----
  NumericVector total_var(max_time);
  double n_squared = (double)n * n;

  double cntLE = 0.0;   // #{c_i <= j}
  double wLE   = 0.0;   // sum (n-i) for c_i <= j
  double SA    = 0.0;   // sum A(c_i - 1)
  double SB    = 0.0;   // sum B(c_i - 1)
  double SWB   = 0.0;   // sum (n-i) * B(c_i - 1)

  for (int j = 1; j <= max_time; j++) {
    // add subjects whose censor time is exactly j
    cntLE += cnt[j];
    wLE   += wsum[j];
    SA    += cnt[j]  * A[j - 1];
    SB    += cnt[j]  * B[j - 1];
    SWB   += wsum[j] * B[j - 1];

    double term1 = A[j] * (n - cntLE) + SA;
    double term2 = B[j] * cntLE - SB;
    double term3 = B[j] * wLE   - SWB;

    total_var[j - 1] = (s_sq[j - 1] / n_squared) * (term1 + term2 + 2.0 * term3);
    p.increment();
  }


  return total_var;
}
