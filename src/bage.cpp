
#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS
#include <TMB.hpp>
#include "init.h"

using namespace density;
using namespace Eigen;
using namespace tmbutils;


// Functions for calculating prior density ------------------------------------

// Functions to calculation log-density of priors
// for main effects and interactions.
// Assume inputs all valid (checking done in R).
// Note that the 'Known' prior does not have
// an associated 'logpost' function.

template <class Type>
Type logpost_norm(vector<Type> parfree,
		  vector<Type> hyper,
		  vector<Type> consts) {
  Type scale = consts[0];
  Type log_sd = hyper[0];
  Type sd = exp(log_sd);
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  ans += dnorm(parfree, Type(0), sd, true).sum();
  return ans;
}

template <class Type>
Type logpost_normfixed(vector<Type> parfree,
  		       vector<Type> hyper,
		       vector<Type> consts) {
  Type sd = consts[0];
  Type ans = 0;
  ans += dnorm(parfree, Type(0), sd, true).sum();
  return ans;
}

template <class Type>
Type logpost_rw(vector<Type> parfree,
		vector<Type> hyper,
		vector<Type> consts) {
  return logpost_norm(parfree, hyper, consts);
}

template <class Type>
Type logpost_rw2(vector<Type> parfree,
		 vector<Type> hyper,
		 vector<Type> consts) {
  Type sd_slope = consts[0];
  Type scale = consts[1];
  Type log_sd = hyper[0];
  Type sd = exp(log_sd);
  Type ans = 0;
  ans += dnorm(parfree[0], Type(0), sd_slope, true);
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  for (int i = 1; i < parfree.size(); i++)
    ans += dnorm(parfree[i], Type(0), sd, true);
  return ans;
}

template <class Type>
Type logpost_ar1(vector<Type> parfree,
		 vector<Type> hyper,
		 vector<Type> consts) {
  Type shape1 = consts[0];
  Type shape2 = consts[1];
  Type min = consts[2];
  Type max = consts[3];
  Type scale = consts[4];
  Type logit_coef = hyper[0];
  Type log_sd = hyper[1];
  Type coef_raw = exp(logit_coef) / (1 + exp(logit_coef));
  Type coef = (max - min) * coef_raw + min;
  Type sd = exp(log_sd);
  Type ans = 0;
  ans += dbeta(coef_raw, shape1, shape2, true)
    + log(coef_raw) + log(1 - coef_raw);
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  ans -= SCALE(AR1(coef), sd)(parfree); // AR1 returns neg log-lik
  return ans;
}

template <class Type>
Type logpost(vector<Type> parfree,
	     vector<Type> hyper,
	     vector<Type> consts,
	     int i_prior) {
  Type ans = 0;
  switch(i_prior) {
  case 1:
    ans = logpost_norm(parfree, hyper, consts);
    break;
  case 2:
    ans = logpost_normfixed(parfree, hyper, consts);
    break;
  case 3:
    ans = logpost_rw(parfree, hyper, consts);
    break;
  case 4:
    ans = logpost_rw2(parfree, hyper, consts);
    break;
  case 5:
    ans = logpost_ar1(parfree, hyper, consts);
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
  DATA_IVECTOR(is_in_lik);
  DATA_FACTOR(terms_parfree);       
  DATA_STRUCT(matrices_parfree_outcome, LIST_SM_t); 
  DATA_IVECTOR(i_prior);        
  DATA_FACTOR(terms_hyper);
  DATA_VECTOR(consts);
  DATA_FACTOR(terms_consts);

  PARAMETER_VECTOR(parfree);        
  PARAMETER_VECTOR(hyper);
  

  // intermediate quantities

  int n_outcome = outcome.size();
  int n_term = i_prior.size();
  vector<vector<Type> > parfree_split = split(parfree, terms_parfree);       
  vector<vector<Type> > hyper_split = split(hyper, terms_hyper); 
  vector<vector<Type> > consts_split = split(consts, terms_consts); 

  vector<Type> linear_pred(n_outcome);
  linear_pred.fill(0);
  for (int i_term = 0; i_term < n_term; i_term++) {
    SparseMatrix<Type> matrix_parfree_outcome = matrices_parfree_outcome[i_term];
    vector<Type> parfree_term = parfree_split[i_term];
    linear_pred = linear_pred + matrix_parfree_outcome * parfree_term;
  }


  // negative log posterior
  
  Type ans = 0;

  // contribution to log posterior from priors
  for (int i_term = 0; i_term < n_term; i_term++) {
    int i_prior_term = i_prior[i_term];
    if (i_prior_term > 0) { // i_prior_term == 0 when prior is "Known"
      vector<Type> parfree_term = parfree_split[i_term];
      vector<Type> hyper_term = hyper_split[i_term];
      vector<Type> consts_term = consts_split[i_term];
      ans -= logpost(parfree_term, hyper_term, consts_term, i_prior_term);
    }
  }

  // contribution to log posterior from data
  if (nm_distn == "pois") {
    for (int i_outcome = 0; i_outcome < n_outcome; i_outcome++) {
      if (is_in_lik[i_outcome]) {
	Type rate = exp(linear_pred[i_outcome]);
	ans -= dpois(outcome[i_outcome],
		     rate * offset[i_outcome],
		     true);
      }
    }
  }
  else if (nm_distn == "binom") {
    for (int i_outcome = 0; i_outcome < n_outcome; i_outcome++) {
      if (is_in_lik[i_outcome]) {
	ans -= dbinom_robust(outcome[i_outcome],
			     offset[i_outcome],
			     linear_pred[i_outcome],
			     true);
      }
    }
  }
  else { // norm
    for (int i_outcome = 0; i_outcome < n_outcome; i_outcome++) {
      if (is_in_lik[i_outcome]) {
	ans -= dnorm(outcome[i_outcome],
		     linear_pred[i_outcome],
		     1 / offset[i_outcome],
		     true);
      }
    }
  }

  return ans;
}
