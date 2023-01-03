
#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS
#include <TMB.hpp>
#include "init.h"

using namespace Eigen;
using namespace tmbutils;


// Functions for calculating prior density ------------------------------------

// Functions to calculation log-density of priors
// for main effects and interactions.
// Assume inputs all valid (checking done in R).

template <class Type>
Type logpost_norm(vector<Type> par,
		  vector<Type> hyper,
		  vector<Type> consts) {
  Type log_sd = hyper[0];
  Type scale = consts[0];
  Type sd = exp(log_sd);
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  ans += dnorm(par, Type(0), sd, true).sum();
  return ans;
}

template <class Type>
Type logpost_rw(vector<Type> par,
		vector<Type> hyper,
		vector<Type> consts) {
  Type log_sd = hyper[0];
  Type scale = consts[0];
  int n = par.size();
  Type sd = exp(log_sd);
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  for (int i = 1; i < n; i++) {
    Type diff = par[i] - par[i - 1];
    ans += dnorm(diff, Type(0), sd, true);
  }
  return ans;
}

template <class Type>
Type logpost_rw2(vector<Type> par,
		 vector<Type> hyper,
		 vector<Type> consts) {
  Type log_sd = hyper[0];
  Type scale = consts[0];
  int n = par.size();
  Type sd = exp(log_sd);
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  for (int i = 2; i < n; i++) {
    Type diff = par[i] - 2 * par[i - 1] + par[i - 2];
    ans += dnorm(diff, Type(0), sd, true);
  }
  return ans;
}

template <class Type>
Type logpost(vector<Type> par,
	     vector<Type> hyper,
	     vector<Type> consts,
	     int i_prior) {
  Type ans = 0;
  switch(i_prior) {
  case 1:
    ans = logpost_norm(par, hyper, consts);
    break;
  case 2:
    ans = logpost_rw(par, hyper, consts);
    break;
  case 3:
    ans = logpost_rw2(par, hyper, consts);
    break;
  default:
    error("function 'logpost' cannot handle i_prior = %d", i_prior);
  }
  return ans;
}


// List object to hold map matrices -------------------------------------------

// Modified from code at https://github.com/kaskr/adcomp/issues/96

template<class Type>
struct LIST_SM_t : vector<SparseMatrix<Type> > {
  LIST_SM_t(SEXP x){
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

  // inputs

  DATA_STRING(nm_distn);
  DATA_VECTOR(outcome);
  DATA_VECTOR(offset);
  DATA_FACTOR(term_par);       
  DATA_STRUCT(matrices_par, LIST_SM_t); 
  DATA_IVECTOR(i_prior);        
  DATA_FACTOR(term_hyper);
  DATA_VECTOR(consts);
  DATA_FACTOR(term_consts);

  PARAMETER_VECTOR(par);        
  PARAMETER_VECTOR(hyper);
  

  // intermediate quantities

  int n_outcome = outcome.size();
  int n_term = i_prior.size();
  vector<vector<Type> > par_split = split(par, term_par);       
  vector<vector<Type> > hyper_split = split(hyper, term_hyper); 
  vector<vector<Type> > consts_split = split(consts, term_consts); 

  vector<Type> linear_pred(n_outcome);
  linear_pred.fill(0);
  for (int i_term = 0; i_term < n_term; i_term++) {
    SparseMatrix<Type> matrix_term = matrices_par[i_term];
    vector<Type> par_term = par_split[i_term];
    if (matrix_term.size() > 0) {
      linear_pred = linear_pred + matrix_term * par_term;
    }
    else {
      linear_pred = linear_pred + par_term[0];
    }
  }


  // log posterior
  
  Type ans = 0;

  // contribution to log posterior from priors
  for (int i_term = 0; i_term < n_term; i_term++) {
    vector<Type> par_term = par_split[i_term];
    vector<Type> hyper_term = hyper_split[i_term];
    vector<Type> consts_term = consts_split[i_term];
    int i_prior_term = i_prior[i_term];
    ans -= logpost(par_term, hyper_term, consts_term, i_prior_term);
  }

  // contribution to log posterior from data
  if (nm_distn == "pois") {
    for (int i_outcome = 0; i_outcome < n_outcome; i_outcome++) {
      Type rate = exp(linear_pred[i_outcome]);
      ans -= dpois(outcome[i_outcome],
		   rate * offset[i_outcome],
		   true);
    }
  }
  else if (nm_distn == "binom") {
    for (int i_outcome = 0; i_outcome < n_outcome; i_outcome++) {
      ans -= dbinom_robust(outcome[i_outcome],
			   offset[i_outcome],
			   linear_pred[i_outcome],
			   true);
    }
  }
  else { // norm
    for (int i_outcome = 0; i_outcome < n_outcome; i_outcome++) {
      ans -= dnorm(outcome[i_outcome],
		   linear_pred[i_outcome],
		   1 / offset[i_outcome],
		   true);
    }
  }

  return ans;
}
