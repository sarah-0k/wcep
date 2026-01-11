#include <Rcpp.h>
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
using namespace Rcpp;

// [[Rcpp::export]]

NumericVector varvec(NumericVector W,
                      NumericVector uj,
                      NumericMatrix pj,
                      NumericMatrix s_table_c,
                      IntegerVector censor_times,
                      IntegerVector n_riskset,
                      int n,
                      int max_time,
                      bool display_progress = true) {

  int k = W.size();
  NumericVector term1_j(max_time);
  NumericVector term2_j(max_time);
  NumericVector term4_j(max_time);

  Progress p(max_time*3, display_progress); //each term is 1/3 bar

  // ========== TERM 1 ==========
  for (int j = 0; j < max_time; j++) {

    double sum_i = 0.0;

    for (int i = 0; i < n; i++) {
      double sum_t = 0.0;
      int upper_limit = std::min(j + 1, censor_times[i] - 1);

      for (int t_val = 1; t_val <= upper_limit; t_val++) {
        int t = t_val - 1; // Convert to 0-based index

        // Extract pj[t,]
        NumericVector pj_t(k);
        for (int idx = 0; idx < k; idx++) {
          pj_t[idx] = pj(t, idx);
        }

        // Calculate (1 - uj[t])^(-2)
        double factor = pow(1.0 - uj[t], -2.0);

        // Calculate diag(pj[t,]) - pj[t,] %*% t(pj[t,])
        double quad_form = 0.0;
        for (int row = 0; row < k; row++) {
          for (int col = 0; col < k; col++) {
            double mat_val = (row == col) ? (pj_t[row] - pj_t[row] * pj_t[col]) : (-pj_t[row] * pj_t[col]);
            quad_form += W[row] * mat_val * W[col];
          }
        }

        sum_t += factor * quad_form;
      }

      sum_i += sum_t * s_table_c(i, j) * s_table_c(i, j);
    }

    term1_j[j] = sum_i;

    p.increment(); // update progress
  }

  // ========== TERM 2 ==========
  for (int j = 0; j < max_time; j++) {

    int j_time = j + 1; // Convert to 1-based for comparison

    // Check if any(censor_times <= j_time)
    bool has_censored = false;
    for (int idx = 0; idx < n; idx++) {
      if (censor_times[idx] <= j_time) {
        has_censored = true;
        break;
      }
    }

    if (!has_censored) {
      term2_j[j] = 0.0;
      continue;
    }

    double sum_i = 0.0;

    // Loop over i where censor_times[i] <= j_time
    for (int i = 0; i < n; i++) {
      if (censor_times[i] <= j_time) {

        double sum_t = 0.0;

        // Loop from censor_times[i] to j_time
        for (int t_val = censor_times[i]; t_val <= j_time; t_val++) {
          int t = t_val - 1; // Convert to 0-based index

          // Extract pj[t,]
          NumericVector pj_t(k);
          for (int idx = 0; idx < k; idx++) {
            pj_t[idx] = pj(t, idx);
          }


          // Calculate (1 - uj[t])^(-2)
          double factor = pow(1.0 - uj[t], -2.0);

          // Calculate quadratic form with pj[j,]
          double quad_form = 0.0;
          for (int row = 0; row < k; row++) {
            for (int col = 0; col < k; col++) {
              double mat_val = (row == col) ? (pj_t[row] - pj_t[row] * pj_t[col]) : (-pj_t[row] * pj_t[col]);
              quad_form += W[row] * mat_val * W[col];
            }
          }

          sum_t += (factor * quad_form) / n_riskset[t];
        }

        sum_i += sum_t * s_table_c(i, j) * s_table_c(i, j);
      }
    }

    term2_j[j] = sum_i;

    p.increment(); // update progress
  }

  // ========== TERM 4 ==========

  // Pre-compute censored indices for each time point
  std::vector<std::vector<int>> censored_at_or_before(max_time);
  std::vector<std::vector<int>> censored_after(max_time);

  for (int j = 0; j < max_time; j++) {
    int j_time = j + 1;
    censored_at_or_before[j].reserve(n);
    censored_after[j].reserve(n);

    for (int i = 0; i < n; i++) {
      if (censor_times[i] <= j_time) {
        censored_at_or_before[j].push_back(i);
      } else {
        censored_after[j].push_back(i);
      }
    }
  }

  // Pre-compute W outer product (symmetric matrix)
  std::vector<double> W_outer(k * k);
  for (int row = 0; row < k; row++) {
    for (int col = 0; col < k; col++) {
      W_outer[row * k + col] = W[row] * W[col];
    }
  }

  for (int j = 0; j < max_time; j++) {
    if (censored_at_or_before[j].empty()) {
      term4_j[j] = 0.0;
      continue;
    }

    double sum_i = 0.0;

    for (int i : censored_at_or_before[j]) {
      int censor_i = censor_times[i];
      double s_table_c_ij = s_table_c(i, j);
      double sum_ip = 0.0;

      for (int ip : censored_after[j]) {
        double s_table_c_ipj = s_table_c(ip, j);
        double sum_t = 0.0;

        // Loop from censor_times[i] to j_time
        for (int t_val = censor_i; t_val <= j + 1; t_val++) {
          int t = t_val - 1;

          // Pre-fetch common values
          double uj_t = uj[t];
          double factor = 1.0 / ((1.0 - uj_t) * (1.0 - uj_t));
          double n_risk_inv = 1.0 / n_riskset[t];

          // Calculate quadratic form more efficiently
          double quad_form = 0.0;

          // Diagonal terms: pj[row] * (1 - pj[row]) * W[row]^2
          for (int idx = 0; idx < k; idx++) {
            double pj_val = pj(t, idx);
            quad_form += pj_val * (1.0 - pj_val) * W_outer[idx * k + idx];
          }

          // Off-diagonal terms: -2 * pj[row] * pj[col] * W[row] * W[col]
          // (multiply by 2 since matrix is symmetric)
          for (int row = 0; row < k; row++) {
            double pj_row = pj(t, row);
            for (int col = row + 1; col < k; col++) {
              double pj_col = pj(t, col);
              quad_form -= 2.0 * pj_row * pj_col * W_outer[row * k + col];
            }
          }

          sum_t += (factor * quad_form) * n_risk_inv;
        }
        sum_ip += sum_t * s_table_c_ipj;
      }
      sum_i += sum_ip * s_table_c_ij;
    }

    term4_j[j] = sum_i;

    p.increment(); // update progress
  }

  // ========== CALCULATE varvec ==========
  double n_squared = (double)n * n;
  NumericVector varvec(max_time);

  for (int j = 0; j < max_time; j++) {
    varvec[j] = (1.0 / n_squared) * (term1_j[j] + term2_j[j] + 2.0 * term4_j[j]);
  }

  return varvec;
}
