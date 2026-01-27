#include <Rcpp.h>
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
using namespace Rcpp;

// [[Rcpp::export]]

NumericVector varvec(NumericVector W,
                      NumericVector uj,
                      NumericMatrix pj,
                      NumericMatrix s_table,
                      IntegerVector censor_times,
                      IntegerVector n_riskset,
                      int n,
                      int max_time,
                      bool display_progress = true) {

  int k = W.size();
  NumericVector term1_j(max_time);
  NumericVector term2_j(max_time);
  NumericVector term4_j(max_time);

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
      sum_i += sum_t * s_table(i, j) * s_table(i, j);
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
        sum_i += sum_t * s_table(i, j) * s_table(i, j);
      }
    }
    term2_j[j] = sum_i;

    p.increment(); // update progress
  }

  // ========== TERM 3: Covariance Component (OPTIMIZED) ==========
  // Original complexity: O(n^2 * T) -> Reduced to O(n * T)
  // Strategy: Pre-calculate the sum of survival probabilities for "alive" patients.

  for (int j = 0; j < max_time; j++) {
    int j_time = j + 1;

    // Step A: Pre-calculate sum of S(ip, j) for all patients (ip) still in risk set
    double sum_alive_survival = 0.0;
    for (int ip = 0; ip < n; ip++) {
      if (censor_times[ip] > j_time) {
        sum_alive_survival += s_table(ip, j);
      }
    }

    // If risk set is empty, Term 4 is zero
    if (sum_alive_survival == 0.0) {
      term4_j[j] = 0.0;
      continue;
    }

    double sum_i = 0.0;

    // Step B: Loop only through censored patients (i)
    for (int i = 0; i < n; i++) {
      if (censor_times[i] <= j_time) {

        double sum_t = 0.0;
        // Calculate integral (dependent only on i)
        for (int t_val = censor_times[i]; t_val <= j_time; t_val++) {
          int t = t_val - 1;

          // --- Matrix Multiplication Logic ---
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
          sum_t += (factor * quad_form) / n_riskset[t];
        }

        // Optimized Formula: sum_t * s_table(i, j) * (Sum of Alive Survival)
        sum_i += sum_t * s_table(i, j) * sum_alive_survival;
      }
    }
    term4_j[j] = sum_i;

    p.increment();
  }

  // ========== Final Variance Aggregation ==========
  double n_squared = (double)n * n;
  NumericVector total_var(max_time);

  for (int j = 0; j < max_time; j++) {
    total_var[j] = (1.0 / n_squared) * (term1_j[j] + term2_j[j] + 2.0 * term4_j[j]);

    p.increment(); // update progress
  }

  return total_var;
}
