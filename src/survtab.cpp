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
  NumericMatrix lifemat(n, t), nocens(n, t), weight(m,n);

  Progress p(t, display_progress);

  for(int j = 0; j < t; ++j) {

    for(int i = 0; i < n; ++i) {

      lifemat(i,j) = 1;
      nocens(i,j) = 1;

      for(int v = 0; v < m; ++v) {

        weight(v,i) = (Xr(v,i) <= j+1 ? (1 - Xr(v,n)) : 1);
        nocens(i,j) = nocens(i,j) * weight(v, i);
      }

      if(j < (C[i]-1)){

        lifemat(i,j) = nocens(i,j);

      }

      else {

        lifemat(i,j) = lifemat(i,j-1) * (1 - U[j]);

      }
    }
    p.increment(); // update progress
  }

  return lifemat;
}

