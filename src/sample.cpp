/*

#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS
#include <TMB.hpp>

using namespace Eigen;
using namespace tmbutils;


// Log-Posterior from Prior Models --------------------------------------------

enum i_prior {
  norm =         1,
  rw_1 =        10,
  rw_2_indep =  11,
  rw_2_ar1 =    12,
  rw_3_indep =  13,
  rw_3_ar1 =    14,
  rw2_1 =       20,
  rw2_2_indep = 21,
  rw2_2_ar1 =   22,
  rw2_3_indep = 23,
  rw2_3_ar1 =   24,
  ar1_1 =       30,
  ar1_2_indep = 31,
  ar1_2_ar1 =   32,
  ar1_3_indep = 33,
  ar1_3_ar1 =   34,
  lin_1 =       40
};


// Functions to calculation log-density of priors
// for main effects and interactions.
// Assume inputs all valid (checking done in R).

template <class Type>
Type logpost_norm(vector<Type> par, vector<Type> hyper sd) {
  Type log_sd = hyper[0];
  Type scale = hyper[1];
  Type sd = exp(log_sd);
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  ans += dnorm(par, Type(0), sd, true).sum();
  return ans;
}

template <class Type>
Type logpost_rw1(vector<Type> par, vector<Type> hyper) {
  Type log_sd = hyper[0];
  Type scale = hyper[1];
  int n = x.size();
  Type sd = exp(log_sd);
  Type ans = 0;
  for (int i = 1; i < n; i++) {
    Type diff = par[i] - par[i - 1];
    ans += dnorm(diff, Type(0), sd, true);
  }
  return ans;
}

template <class Type>
Type logpost_rw2(vector<Type> x, Type sd) {
  int n = x.size();
  Type ans = 0;
  for (int i = 2; i < n; i++) {
    Type diff = x[i] - 2 * x[i - 1] + x[i - 2];
    ans += dnorm(diff, Type(0), sd, true);
  }
  return ans;
}

template <class Type>
Type logpost_ar1(vector<Type> x, Type coef, Type sd) {
  using namespace density;
  return -1 * SCALE(AR1(coef), sd)(x);
}




template <class Type>
Type logpost(vector<Type> par, vector<Type> hyper, int index_prior) {
  Type ans = 0;
  switch(index_prior) {
  case norm:
    ans = logpost_norm(par, hyper);
    break;
  case rw1:
    ans = logpost_rw1(par, hyper);
    break;
  }
  return ans;
}


// List object to hold map matrices -------------------------------------------

// Modified from code at https://github.com/kaskr/adcomp/issues/96

template<class Type>
struct list_sm_t : vector<SparseMatrix<Type> > {
  list_sm_t(SEXP x){
    (*this).resize(LENGTH(x));
    for (int i = 0; i < LENGTH(x); i++){
      SEXP sm = VECTOR_ELT(x, i);
      (*this)(i) = asSparseMatrix<Type>(sm);
    }
  }
};




// Objective function ---------------------------------------------------------

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(y);
  DATA_VECTOR(w);
  DATA_IVECTOR(index_prior);    // n_term
  DATA_FACTOR(index_par);       // n_par
  DATA_FACTOR(index_hyper);     // n_hyper
  DATA_STRUCTURE(map_matrices); // n_term

  PARAMETER_VECTOR(par);   // n_par
  PARAMETER_VECTOR(hyper); // n_hyper

  int n_term = indices_priors.size();
  int n_y = y.size();
  
  vector<vector<Type> > par_split = split(par, index_par);       // n_term
  vector<vector<Type> > hyper_split = split(hyper, index_hyper); // n_term


  // make linear predictor
  vector<Type> linear_pred(n_y);
  linear_pred.fill(0);
  for (int i_term = 0; i_term < n_term; i_term++) {
    SparseMatrix<Type> M_term = map_matrices[i_term];
    vector<Type> = par_term;
    linear_pred = linear_pred + M_term * par_term;
  }
  
  Type ans = 0;

  // contribution to log posterior from priors
  for (int i_term = 0; i_term < n_term; i++) {
    vector<Type> = par_term = par_split[i_term];
    vector<Type> = hyper_term = par_split[i_term];
    int index_prior_term = index_prior[i_term];
    ans -= logpost(par_term, hyper_term, index_prior_term);
  }

  // contribution to log posterior from data
  for (int i = 0; i < y.size(); i++) {
    Type rate_i = exp(linear_pred[i]);
    ans -= dpois(y[i], rate_i * w[i], true);
  }

  return ans;
}

*/
