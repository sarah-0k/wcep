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

  const int m = Xr.nrow();
  NumericMatrix lifemat(n, t);
  double* L = lifemat.begin();                 // column-major: L[i + j*n]
  std::fill(L, L + (size_t)n * t, 1.0);        // holds per-day event factors first

  const double* X = Xr.begin();                // column-major: X[v + i*m]
  const double* w = X + (size_t)n * m;         // weights live in column n (0-based)


  // initialize progress bar of length t
  Progress p(n + t, display_progress);

  // ---- Pass 1: one scan of the events; contiguous reads down each column ----
  for (int i = 0; i < n; ++i) {
    const double* xi = X + (size_t)i * m;
    for (int v = 0; v < m; ++v) {
      const double tm = xi[v];
      if (tm >= 1.0 && tm <= t) {              // also rejects NaN
        const int d = (int)tm;
        if ((double)d == tm) {                 // same as original equality test
          L[i + (size_t)(d - 1) * n] *= (1.0 - w[v]);
        }
      }
    }
    p.increment();
  }

  // ---- Precompute censoring day per patient (j < cd[i] means not yet censored) ----
  std::vector<int> cd(n);
  for (int i = 0; i < n; ++i) cd[i] = (int)C[i] - 1;

  // ---- Pass 2: day-major sweep with a single running value per patient ----
  // cur[i] = last value written for patient i. Uncensored: multiply by the day's
  // event factor. Censored: multiply by (1 - U[j]). Since the censored recursion
  // starts from lifemat(i, j-1), which equals cur[i], one array is enough.
  std::vector<double> cur(n, 1.0);
  double* c = cur.data();
  const int* cdp = cd.data();


  for (int j = 0; j < t; ++j) {
    double* col = L + (size_t)j * n;           // contiguous column for day j
    const double oneMinusU = 1.0 - U[j];

    for (int i = 0; i < n; ++i) {
      const double mult = (j < cdp[i]) ? col[i] : oneMinusU;
      c[i] *= mult;
      col[i] = c[i];
    }
    p.increment();
  }

  return lifemat;
}

