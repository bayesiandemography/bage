
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
Type logpost_norm(vector<Type> effectfree,
		  vector<Type> hyper,
		  vector<Type> consts,
		  matrix<int> matrix_along_by) {
  Type scale = consts[0];
  Type log_sd = hyper[0];
  Type sd = exp(log_sd);
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  ans += dnorm(effectfree, Type(0), sd, true).sum();
  return ans;
}

template <class Type>
Type logpost_normfixed(vector<Type> effectfree,
		       vector<Type> consts,
		       matrix<int> matrix_along_by) {
  Type sd = consts[0];
  Type ans = 0;
  ans += dnorm(effectfree, Type(0), sd, true).sum();
  return ans;
}

template <class Type>
Type logpost_rw(vector<Type> effectfree,
		vector<Type> hyper,
		vector<Type> consts,
		matrix<int> matrix_along_by) {
  Type scale = consts[0];
  Type log_sd = hyper[0];
  Type sd = exp(log_sd);
  int n = effectfree.size();
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  ans += dnorm(effectfree[0], Type(0), Type(1), true);
  for (int i = 1; i < n; i++) {
    Type diff = effectfree[i] - effectfree[i-1];
    ans += dnorm(diff, Type(0), sd, true);
  }
  return ans;
}

template <class Type>
Type logpost_rw2(vector<Type> effectfree,
		 vector<Type> hyper,
		 vector<Type> consts,
		 matrix<int> matrix_along_by) {
  Type scale = consts[0];
  Type sd_slope = consts[1];
  Type log_sd = hyper[0];
  Type sd = exp(log_sd);
  int n = effectfree.size();
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  ans += dnorm(effectfree[0], Type(0), Type(1), true);
  Type diff = effectfree[1] - effectfree[0];
  ans += dnorm(diff, Type(0), sd_slope, true);
  for (int i = 2; i < n; i++) {
    Type diff = effectfree[i] - 2 * effectfree[i-1] + effectfree[i-2];
    ans += dnorm(diff, Type(0), sd, true);
  }
  return ans;
}

template <class Type>
Type logpost_ar(vector<Type> effectfree,
		vector<Type> hyper,
		vector<Type> consts,
		matrix<int> matrix_along_by) {
  Type shape1 = consts[0];
  Type shape2 = consts[1];
  Type min = consts[2];
  Type max = consts[3];
  Type scale = consts[4];
  int k = hyper.size() - 1;
  vector<Type> logit_coef = hyper.head(k);
  Type log_sd = hyper[k];
  vector<Type> coef_raw = exp(logit_coef) / (1 + exp(logit_coef));
  vector<Type> coef = (max - min) * coef_raw + min;
  Type sd = exp(log_sd);
  Type ans = 0;
  ans += dbeta(coef_raw, shape1, shape2, true).sum()
    + log(coef_raw).sum()
    + log(1 - coef_raw).sum();
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  ans -= SCALE(ARk(coef), sd)(effectfree); // ARk returns neg log-lik
  return ans;
}

template <class Type>
Type logpost_spline(vector<Type> effectfree,
   		    vector<Type> hyper,
		    vector<Type> consts,
		    matrix<int> matrix_along_by) {
  return logpost_rw2(effectfree, hyper, consts, matrix_along_by);
}

template <class Type>
Type logpost_svd(vector<Type> effectfree,
		 vector<Type> consts,
		 matrix<int> matrix_along_by) {
  return dnorm(effectfree, Type(0), Type(1), true).sum();
}

template <class Type>
Type logpost_lin(vector<Type> effectfree,
		 vector<Type> hyper,
		 vector<Type> consts,
		 matrix<int> matrix_along_by) {
  Type scale = consts[0];
  Type sd_slope = consts[1];
  Type slope = hyper[0];
  Type log_sd = hyper[1];
  Type sd = exp(log_sd);
  int n = effectfree.size();
  Type ans = 0;
  ans += dnorm(slope, Type(0), sd_slope, true);
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  Type a0 = -1 * (n + 1) / (n - 1);
  Type a1 = 2 / (n - 1);
  for (int i = 0; i < n; i++) {
    Type q = a0 + a1 * (i + 1);
    ans += dnorm(effectfree[i], q * slope, sd, true);
  }
  return ans;
}

template <class Type>
Type logpost_ilin(vector<Type> effectfree,
		  vector<Type> hyper,
		  vector<Type> consts,
		  matrix<int> matrix_along_by) {
  Type scale = consts[0];
  Type sd_slope = consts[1];
  Type mscale = consts[2];
  int n_by = hyper.size() - 3;
  Type slope = hyper[0];
  vector<Type> mslope = hyper.segment(1, n_by);
  Type log_sd = hyper[n_by+1];
  Type log_msd = hyper[n_by+2];
  Type sd = exp(log_sd);
  Type msd = exp(log_msd);
  int n_along = effectfree.size() / n_by;
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  ans += dnorm(msd, Type(0), mscale, true) + log_msd;
  ans += dnorm(slope, Type(0), sd_slope, true);
  ans += dnorm(mslope, slope, msd, true).sum();
  Type a0 = -1 * (n_along + 1) / (n_along - 1);
  Type a1 = 2 / (n_along - 1);
  for (int i_by = 0; i_by < n_by; i_by++) {
    for (int i_along = 0; i_along < n_along; i_along++) {
      int i = matrix_along_by(i_along, i_by);
      Type q = a0 + a1 * (i_along + 1);
      ans += dnorm(effectfree[i], q * mslope[i_by], sd, true);
    }
  }
  return ans;
}

template <class Type>
Type logpost_not_uses_hyper(vector<Type> effectfree,
			    vector<Type> consts,
			    matrix<int> matrix_along_by,
			    int i_prior) {
  Type ans = 0;
  switch(i_prior) {
  case 2:
    ans = logpost_normfixed(effectfree, consts, matrix_along_by);
    break;
  case 7:
    ans = logpost_svd(effectfree, consts, matrix_along_by);
    break;
  default:
    error("Internal error: function 'logpost_not_uses_hyper' cannot handle i_prior = %d", i_prior);
  }
  return ans;
}

template <class Type>
Type logpost_uses_hyper(vector<Type> effectfree,
			vector<Type> hyper,
			vector<Type> consts,
			matrix<int> matrix_along_by,
			int i_prior) {
  Type ans = 0;
  switch(i_prior) {
  case 1:
    ans = logpost_norm(effectfree, hyper, consts, matrix_along_by);
    break;
  case 3:
    ans = logpost_rw(effectfree, hyper, consts, matrix_along_by);
    break;
  case 4:
    ans = logpost_rw2(effectfree, hyper, consts, matrix_along_by);
    break;
  case 5:
    ans = logpost_ar(effectfree, hyper, consts, matrix_along_by);
    break;
  case 6:
    ans = logpost_spline(effectfree, hyper, consts, matrix_along_by);
    break;
  case 8:
    ans = logpost_lin(effectfree, hyper, consts, matrix_along_by);
    break;
  case 9:
    ans = logpost_ilin(effectfree, hyper, consts, matrix_along_by);
    break;
  default:
    error("Internal error: function 'logpost_uses_hyper' cannot handle i_prior = %d", i_prior);
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


// List objects to hold matrices ----------------------------------------------

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

template<class Type>
struct LIST_M_t : vector<matrix<int> > {
  LIST_M_t(SEXP x){
    (*this).resize(LENGTH(x));
    for (int i = 0; i < LENGTH(x); i++){
      SEXP m = VECTOR_ELT(x, i);
      (*this)(i) = asMatrix<int>(m);
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
  DATA_FACTOR(terms_effect);
  DATA_FACTOR(terms_effectfree);
  DATA_IVECTOR(uses_matrix_effectfree_effect);
  DATA_STRUCT(matrices_effectfree_effect, LIST_SM_t);
  DATA_IVECTOR(uses_offset_effectfree_effect);
  DATA_VECTOR(offsets_effectfree_effect);
  DATA_STRUCT(matrices_effect_outcome, LIST_SM_t);
  DATA_IVECTOR(i_prior);
  DATA_IVECTOR(uses_hyper);
  DATA_FACTOR(terms_hyper);
  DATA_VECTOR(consts);
  DATA_FACTOR(terms_consts);
  DATA_STRUCT(matrices_along_by, LIST_M_t);
  DATA_SCALAR(scale_disp);

  PARAMETER_VECTOR(effectfree); 
  PARAMETER_VECTOR(hyper);
  PARAMETER(log_disp);
  

  // intermediate quantities

  int n_outcome = outcome.size();
  int n_term = i_prior.size();
  vector<vector<Type> > effectfree_split = split(effectfree, terms_effectfree);
  vector<vector<Type> > offsets_effectfree_effect_split = split(offsets_effectfree_effect, terms_effect);
  vector<vector<Type> > hyper_split = split(hyper, terms_hyper); 
  vector<vector<Type> > consts_split = split(consts, terms_consts);
  int has_disp = scale_disp > 0;
  Type disp = has_disp ? exp(log_disp) : 0;


  // linear predictor

  vector<Type> linear_pred(n_outcome);
  linear_pred.fill(0);
  for (int i_term = 0; i_term < n_term; i_term++) {
    SparseMatrix<Type> matrix_effect_outcome = matrices_effect_outcome[i_term];
    vector<Type> effectfree_term = effectfree_split[i_term];
    int n_effect = matrix_effect_outcome.cols();
    vector<Type> effect_term(n_effect);
    if (uses_matrix_effectfree_effect[i_term]) {
      SparseMatrix<Type> matrix_effectfree_effect = matrices_effectfree_effect[i_term];
      effect_term = matrix_effectfree_effect * effectfree_term;
    }
    else {
      effect_term = effectfree_term;
    }
    if (uses_offset_effectfree_effect[i_term]) {
      vector<Type> offset_term = offsets_effectfree_effect_split[i_term];
      effect_term = effect_term + offset_term;
    }
    linear_pred = linear_pred + matrix_effect_outcome * effect_term;
  }

  // negative log posterior
  
  Type ans = 0;

  // contribution to log posterior from priors
  for (int i_term = 0; i_term < n_term; i_term++) {
    int i_prior_term = i_prior[i_term];
    if (i_prior_term > 0) { // i_prior_term == 0 when prior is "Known"
      vector<Type> effectfree_term = effectfree_split[i_term];
      vector<Type> consts_term = consts_split[i_term];
      matrix<int> matrix_along_by = matrices_along_by[i_term];
      if (uses_hyper[i_term]) {
	vector<Type> hyper_term = hyper_split[i_term];
	ans -= logpost_uses_hyper(effectfree_term,
				  hyper_term,
				  consts_term,
				  matrix_along_by,
				  i_prior_term);
      }
      else
	ans -= logpost_not_uses_hyper(effectfree_term,
				      consts_term,
				      matrix_along_by,
				      i_prior_term);
    }
  }

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
