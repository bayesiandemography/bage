
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
  Type scale = consts[0];
  Type sd_intercept = consts[1];
  Type log_sd = hyper[0];
  Type sd = exp(log_sd);
  int n = parfree.size();
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  for (int i = 1; i < n; i++) {
    Type diff = parfree[i] - parfree[i-1];
    ans += dnorm(diff, Type(0), sd, true);
  }
  Type parfree_total = parfree.sum();
  ans += dnorm(parfree_total, Type(0), Type(n * sd_intercept), true);
  return ans;
}

template <class Type>
Type logpost_rw2(vector<Type> parfree,
		 vector<Type> hyper,
		 vector<Type> consts) {
  Type scale = consts[0];
  Type sd_intercept = consts[1];
  Type sd_slope = consts[2];
  Type log_sd = hyper[0];
  Type sd = exp(log_sd);
  int n = parfree.size();
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  for (int i = 2; i < n; i++) {
    Type diff = parfree[i] - 2 * parfree[i-1] + parfree[i-2];
    ans += dnorm(diff, Type(0), sd, true);
  }
  Type parfree_total = parfree.sum();
  ans += dnorm(parfree_total, Type(0), Type(n * sd_intercept), true);
  Type num = 0;
  Type den = 0;
  for (int i = 0; i < n; i++) {
    Type h = -1 + i * 2 / (n - 1);
    num += parfree[i] * h;
    den += h * h;
  }
  Type slope = num / den;
  ans += dnorm(slope, Type(0), sd_slope, true);
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
Type logpost_spline(vector<Type> parfree,
   		    vector<Type> hyper,
		    vector<Type> consts) {
  return logpost_rw2(parfree, hyper, consts);
}


template <class Type>
Type logpost_svd(vector<Type> parfree,
		 vector<Type> consts) {
  return dnorm(parfree, Type(0), Type(1), true).sum();
}

template <class Type>
Type logpost_not_uses_hyper(vector<Type> parfree,
			    vector<Type> consts,
			    int i_prior) {
  Type ans = 0;
  switch(i_prior) {
  case 2:
    ans = logpost_normfixed(parfree, consts);
    break;
  case 7:
    ans = logpost_svd(parfree, consts);
    break;
  default:
    error("Internal error: function 'logpost_not_uses_hyper' cannot handle i_prior = %d", i_prior);
  }
  return ans;
}

template <class Type>
Type logpost_uses_hyper(vector<Type> parfree,
			vector<Type> hyper,
			vector<Type> consts,
			int i_prior) {
  Type ans = 0;
  switch(i_prior) {
  case 1:
    ans = logpost_norm(parfree, hyper, consts);
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
  case 6:
    ans = logpost_spline(parfree, hyper, consts);
    break;
  default:
    error("Internal error: function 'logpost_uses_hyper' cannot handle i_prior = %d", i_prior);
  }
  return ans;
}

template <class Type>
Type logpost_season(vector<Type> par_season,
		    vector<Type> hyper_season,
		    vector<Type> consts_season,
		    int n_time,
		    int n_season) {
  Type scale = consts_season[0];
  Type sd_intercept = consts_season[1];
  Type log_sd = hyper_season[0];
  Type sd = exp(log_sd);
  int n_par = par_season.size();
  int n_by = n_par / n_time;
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  for (int i_by = 0; i_by < n_by; i_by++) {
    for (int i_time = n_season; i_time < n_time; i_time++) {
      int idx_curr = i_by + i_time * n_by;
      int idx_prev = i_by + (i_time - n_season) * n_by;
      Type diff = par_season[idx_curr] - par_season[idx_prev];
      ans += dnorm(diff, Type(0), sd, true);
    }
    Type sum_by = 0;
    for (int i_time = 0; i_time < n_time; i_time ++) {
      int idx = i_by + i_time * n_by;
      sum_by += par_season[idx];
    }
    ans += dnorm(sum_by, Type(0), Type(n_time * sd_intercept), true);
  }
  return ans;
}

template <class Type>
Type logpost_betabinom(Type x,
		       Type n,
		       Type logit_mu,
		       Type disp) {
  Type mu = 1 / (1 + exp(-logit_mu));
  Type alpha = mu / disp;
  Type beta = (1 - mu) / disp;
  Type log_num = lgamma(x + alpha) + lgamma(n - x + beta) - lgamma(n + alpha + beta);
  Type log_den = lgamma(alpha) + lgamma(beta) - lgamma(alpha + beta);
  return log_num - log_den;
}


// List object to hold matrices -----------------------------------------------

// Modified from code at https://github.com/kaskr/adcomp/issues/96

template<class Type>
struct LIST_SM_t : vector<SparseMatrix<Type> > {
  LIST_SM_t(SEXP x){
    (*this).resize(LENGTH(x));
    for (int i = 0; i < LENGTH(x); i++){
      SEXP sm = VECTOR_ELT(x, i);
      if(!isValidSparseMatrix(sm))
        error("Internal error: not a sparse matrix");
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
  DATA_FACTOR(terms_par);
  DATA_FACTOR(terms_parfree);
  DATA_IVECTOR(uses_matrix_parfree_par);
  DATA_STRUCT(matrices_parfree_par, LIST_SM_t);
  DATA_IVECTOR(uses_offset_parfree_par);
  DATA_VECTOR(offsets_parfree_par);
  DATA_STRUCT(matrices_par_outcome, LIST_SM_t);
  DATA_IVECTOR(i_prior);
  DATA_IVECTOR(uses_hyper);
  DATA_FACTOR(terms_hyper);
  DATA_VECTOR(consts);
  DATA_FACTOR(terms_consts);
  DATA_SCALAR(scale_disp);
  DATA_INTEGER(n_time);
  DATA_INTEGER(n_season);
  DATA_VECTOR(consts_season);
  DATA_SPARSE_MATRIX(matrix_season_outcome);

  PARAMETER_VECTOR(parfree); 
  PARAMETER_VECTOR(hyper);
  PARAMETER(log_disp);
  PARAMETER_VECTOR(par_season);
  PARAMETER_VECTOR(hyper_season);
  

  // intermediate quantities

  int n_outcome = outcome.size();
  int n_term = i_prior.size();
  vector<vector<Type> > parfree_split = split(parfree, terms_parfree);
  vector<vector<Type> > offsets_parfree_par_split = split(offsets_parfree_par, terms_par);
  vector<vector<Type> > hyper_split = split(hyper, terms_hyper); 
  vector<vector<Type> > consts_split = split(consts, terms_consts);
  int has_disp = scale_disp > 0;
  Type disp = has_disp ? exp(log_disp) : 0;
  int has_season = n_season > 0;


  // linear predictor

  vector<Type> linear_pred(n_outcome);
  linear_pred.fill(0);
  for (int i_term = 0; i_term < n_term; i_term++) {
    SparseMatrix<Type> matrix_par_outcome = matrices_par_outcome[i_term];
    vector<Type> parfree_term = parfree_split[i_term];
    int n_par = matrix_par_outcome.cols();
    vector<Type> par_term(n_par);
    if (uses_matrix_parfree_par[i_term]) {
      SparseMatrix<Type> matrix_parfree_par = matrices_parfree_par[i_term];
      par_term = matrix_parfree_par * parfree_term;
    }
    else {
      par_term = parfree_term;
    }
    if (uses_offset_parfree_par[i_term]) {
      vector<Type> offset_term = offsets_parfree_par_split[i_term];
      par_term = par_term + offset_term;
    }
    linear_pred = linear_pred + matrix_par_outcome * par_term;
  }
  if (has_season) {
    linear_pred = linear_pred + matrix_season_outcome * par_season;
  }


  // negative log posterior
  
  Type ans = 0;

  // contribution to log posterior from priors
  for (int i_term = 0; i_term < n_term; i_term++) {
    int i_prior_term = i_prior[i_term];
    if (i_prior_term > 0) { // i_prior_term == 0 when prior is "Known"
      vector<Type> parfree_term = parfree_split[i_term];
      vector<Type> consts_term = consts_split[i_term];
      if (uses_hyper[i_term]) {
	vector<Type> hyper_term = hyper_split[i_term];
	ans -= logpost_uses_hyper(parfree_term, hyper_term, consts_term, i_prior_term);
      }
      else
	ans -= logpost_not_uses_hyper(parfree_term, consts_term, i_prior_term);
    }
  }

  // contribution to log posterior from seasonal effect
  if (has_season)
    ans -= logpost_season(par_season,
			  hyper_season,
			  consts_season,
			  n_time,
			  n_season);

  // contribution to log posterior from dispersion term
  if (has_disp) {
    if ((nm_distn == "pois") || (nm_distn == "binom"))
      ans -= -1 * scale_disp * sqrt(disp) - 0.5 * log_disp;
    else if (nm_distn == "norm")
      ans -= dexp(disp, scale_disp, true);
    else
      error("Internal error: invalid 'nm_distn' in logposterior disp");
    ans -= log_disp; // Jacobian
  }
	     

  // contribution to log posterior from data
  for (int i_outcome = 0; i_outcome < n_outcome; i_outcome++) {
    if (is_in_lik[i_outcome]) {
      Type outcome_i = outcome[i_outcome];
      Type linear_pred_i = linear_pred[i_outcome];
      Type offset_i = offset[i_outcome];
      if (nm_distn == "pois") {
	Type rate_i = exp(linear_pred_i) * offset_i;
	if (has_disp) {
	  Type size = 1 / disp;
	  Type prob_i = size / (rate_i + size);
	  ans -= dnbinom(outcome_i, size, prob_i, true);
	}
	else {
	  ans -= dpois(outcome_i, rate_i, true);
	}
      }
      else if (nm_distn == "binom") {
	if (has_disp) {
	  ans -= logpost_betabinom(outcome_i, offset_i, linear_pred_i, disp);
	}
	else {
	  ans -= dbinom_robust(outcome_i, offset_i, linear_pred_i, true);
	}
      }
      else if (nm_distn == "norm") {
	Type sd_i = disp / sqrt(offset_i);
	ans -= dnorm(outcome_i, linear_pred_i, sd_i, true);
      }
      else {
	error("Internal error: invalid 'nm_distn' in logpost data");
      }
    }
  }

  return ans;
}
