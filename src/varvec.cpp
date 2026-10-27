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
  NumericVector term1_j(max_time);
  NumericVector term2_j(max_time);
  NumericVector term3_j(max_time);

  // Helper vector to avoid repeated memory allocation in loops
  NumericVector pj_t(k);

  //variance progress bar where each term is 1/3 bar plus aggregate portion
  Progress p(max_time*4, display_progress);

  // ========== TERM 1: Greenwood-like component ==========
  for (int j = 0; j < max_time; j++) {
    double sum_i = 0.0;

    for (int i = 0; i < n; i++) {
      double sum_t = 0.0;
      int upper_limit = std::min(j + 1, censor_times[i] - 1);

      for (int t_val = 1; t_val <= upper_limit; t_val++) {
        int t = t_val - 1; // 0-based index

        // --- Matrix Multiplication Logic ---
        for (int idx = 0; idx < k; idx++) pj_t[idx] = pj(t, idx);
        double factor = pow(1.0 - uj[t], -2.0);

        // Quadratic Form Calculation: t(W) %*% (diag(p) - p %*% t(p)) %*% W
        double quad_form = 0.0;
        for (int row = 0; row < k; row++) {
          double temp_row = 0.0;
          for (int col = 0; col < k; col++) {
            double mat_val = (row == col) ? (pj_t[row] - pj_t[row] * pj_t[col]) : (-pj_t[row] * pj_t[col]);
            temp_row += mat_val * W[col];
          }
          quad_form += W[row] * temp_row;
        }
        sum_t += factor * quad_form;
      }
      sum_i += sum_t;
    }
    term1_j[j] = sum_i;

    p.increment(); // update progress
  }

  // ========== TERM 2: Correction for Censoring ==========
  for (int j = 0; j < max_time; j++) {
    int j_time = j + 1;
    double sum_i = 0.0;

    for (int i = 0; i < n; i++) {
      // Only process if patient is censored on or before current time
      if (censor_times[i] <= j_time) {
        double sum_t = 0.0;

        // Integrate from censoring time to current time
        for (int t_val = censor_times[i]; t_val <= j_time; t_val++) {
          int t = t_val - 1;

          // --- Matrix Multiplication Logic (Same as Term 1) ---
          for (int idx = 0; idx < k; idx++) pj_t[idx] = pj(t, idx);
          double factor = pow(1.0 - uj[t], -2.0);

          double quad_form = 0.0;
          for (int row = 0; row < k; row++) {
            double temp_row = 0.0;
            for (int col = 0; col < k; col++) {
              double mat_val = (row == col) ? (pj_t[row] - pj_t[row] * pj_t[col]) : (-pj_t[row] * pj_t[col]);
              temp_row += mat_val * W[col];
            }
            quad_form += W[row] * temp_row;
          }
          // Note: Divided by n_riskset[t] here
          sum_t += (factor * quad_form) / n_riskset[t];
        }
        sum_i += sum_t;
      }
    }
    term2_j[j] = sum_i;

    p.increment(); // update progress
  }

  // ========== TERM 3: Covariance Component  ==========
  // Updated to include covariance between censored and uncensored observations
  for (int j = 0; j < max_time; j++) {
    int j_time = j + 1;
    double sum_i = 0.0;
    for (int i = 0; i < n; i++) {
      // Only process if patient is censored on or before current time
      if (censor_times[i] <= j_time) {
        double sum_t = 0.0;
        // Integrate from censoring time to current time
        for (int t_val = censor_times[i]; t_val <= j_time; t_val++) {
          int t = t_val - 1;
          // --- Matrix Multiplication Logic (Same as Term 1) ---
          for (int idx = 0; idx < k; idx++) pj_t[idx] = pj(t, idx);
          double factor = pow(1.0 - uj[t], -2.0);
          double quad_form = 0.0;
          for (int row = 0; row < k; row++) {
            double temp_row = 0.0;
            for (int col = 0; col < k; col++) {
              double mat_val = (row == col) ? (pj_t[row] - pj_t[row] * pj_t[col]) : (-pj_t[row] * pj_t[col]);
              temp_row += mat_val * W[col];
            }
            quad_form += W[row] * temp_row;
          }
          // Note: Divided by n_riskset[t] here
          sum_t += (factor * quad_form) / n_riskset[t];
        }

        sum_i += sum_t * (n-i);
      }
    }
    term3_j[j] = sum_i;

    p.increment();
  }

  // ========== Final Variance Aggregation ==========
  double n_squared = (double)n * n;
  NumericVector total_var(max_time);

  for (int j = 0; j < max_time; j++) {
    total_var[j] = (s_sq[j] / n_squared) * (term1_j[j] + term2_j[j] + 2.0 * term3_j[j]);

    p.increment(); // update progress
  }

  return total_var;
}
