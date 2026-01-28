#include <Rcpp.h>
// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector survtab(NumericMatrix Xr,
                      int n,
                      int t,
                      NumericVector C,
                      NumericVector U,
                      bool display_progress = true) {

  double m  = Xr.nrow();
  // Optimization: "nocens" matrix removed to save memory.
  // We only need the current cumulative probability state.
  NumericMatrix lifemat(n, t);
  NumericVector current_nocens(n, 1.0); // Tracks "no-censoring" survival prob

  // initialize progress bar of length t
  Progress p(t, display_progress);

  // Loop through time (j) and patients (i)
  for(int j = 0; j < t; ++j) {

    for(int i = 0; i < n; ++i) {

      // Check if any event occurred at time j+1 for patient i
      for(int v = 0; v < m; ++v) {
        // Xr stores event times. Check if event time matches current day
        if (Xr(v,i) == j + 1) {
          // Apply weight penalty: S(t) = S(t-1) * (1 - weight)
          current_nocens[i] *= (1.0 - Xr(v,n));
        }
      }

      // Censoring Logic
      if(j < (C[i]-1)){
        // Patient is NOT yet censored: use calculated probability
        lifemat(i,j) = current_nocens[i];
      }
      else {
        // Patient IS censored: Impute using Mean Risk (U) of the risk set
        // Recursive formula: Prev_Day * (1 - Mean_Risk)
        double prev = (j == 0) ? 1.0 : lifemat(i,j-1);
        lifemat(i,j) = prev * (1.0 - U[j]);
      }
    }
    p.increment(); // update progress
  }

  return lifemat;
}

