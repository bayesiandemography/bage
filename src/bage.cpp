
#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS
#include <TMB.hpp>
#include "init.h"

using namespace density;
using namespace Eigen;
using namespace tmbutils;


// Helper functions -----------------------------------------------------------

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


// "Methods" for 'logpost' function  ------------------------------------------

// Assume inputs all valid (checking done in R).
// Note that the 'Known' prior does not have
// an associated 'logpost' method.

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
  int n_along = matrix_along_by.rows();
  int n_by = matrix_along_by.cols();
  int n_coef = hyper.size() - 1;
  vector<Type> logit_coef = hyper.head(n_coef);
  Type log_sd = hyper[n_coef];
  vector<Type> coef_raw = exp(logit_coef) / (1 + exp(logit_coef));
  vector<Type> coef = (max - min) * coef_raw + min;
  Type sd = exp(log_sd);
  Type ans = 0;
  Type radius = sqrt((coef_raw * coef_raw).sum());
  ans += dbeta(radius, shape1, shape2, true)
    + log(coef_raw).sum()
    + log(1 - coef_raw).sum();
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  for (int i_by = 0; i_by < n_by; i_by++) {
    vector<Type> effect_along(n_along);
    for (int i_along = 0; i_along < n_along; i_along++) {
      int i = matrix_along_by(i_along, i_by);
      effect_along[i_along] = effectfree[i];
    }
    ans -= SCALE(ARk(coef), sd)(effect_along); // ARk returns neg log-lik
  }
  return ans;
}

template <class Type>
Type logpost_lin(vector<Type> effectfree,
		 vector<Type> hyper,
		 vector<Type> hyperrand,
		 vector<Type> consts,
		 matrix<int> matrix_along_by) {
  Type scale = consts[0];
  Type sd_slope = consts[1];
  int n_along = matrix_along_by.rows();
  int n_by = matrix_along_by.cols();
  Type log_sd = hyper[0];
  vector<Type> slope = hyperrand;
  Type sd = exp(log_sd);
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  ans += dnorm(slope, Type(0), sd_slope, true).sum();
  Type a0 = -1 * (n_along + 1.0) / (n_along - 1.0);
  Type a1 = 2 / (n_along - 1.0);
  for (int i_by = 0; i_by < n_by; i_by++) {
    for (int i_along = 0; i_along < n_along; i_along++) {
      int i = matrix_along_by(i_along, i_by);
      Type q = a0 + a1 * (i_along + 1.0);
      ans += dnorm(effectfree[i], q * slope[i_by], sd, true);
    }
  }
  return ans;
}

template <class Type>
Type logpost_linar(vector<Type> effectfree,
		   vector<Type> hyper,
		   vector<Type> hyperrand,
		   vector<Type> consts,
		   matrix<int> matrix_along_by) {
  Type scale = consts[0];
  Type sd_slope = consts[1];
  Type shape1 = consts[2];
  Type shape2 = consts[3];
  Type min = consts[4];
  Type max = consts[5];
  Type log_sd = hyper[0];
  int n_coef = hyper.size() - 1;
  vector<Type> logit_coef = hyper.segment(1, n_coef);
  vector<Type> slope = hyperrand;
  int n_along = matrix_along_by.rows();
  int n_by = matrix_along_by.cols();
  Type sd = exp(log_sd);
  vector<Type> coef_raw = exp(logit_coef) / (1 + exp(logit_coef));
  vector<Type> coef = (max - min) * coef_raw + min;
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  ans += dnorm(slope, Type(0), sd_slope, true).sum();
  Type radius = sqrt((coef_raw * coef_raw).sum());
  ans += dbeta(radius, shape1, shape2, true)
    + log(coef_raw).sum()
    + log(1 - coef_raw).sum();
  Type a0 = -1 * (n_along + 1.0) / (n_along - 1.0);
  Type a1 = 2 / (n_along - 1.0);
  for (int i_by = 0; i_by < n_by; i_by++) {
    vector<Type> err(n_along);
    for (int i_along = 0; i_along < n_along; i_along++) {
      Type q = a0 + a1 * (i_along + 1.0);
      int i = matrix_along_by(i_along, i_by);
      err[i_along] = effectfree[i] - q * slope[i_by];
    }
    ans -= SCALE(ARk(coef), sd)(err); // ARk returns neg log-lik
  }
  return ans;
}

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
  int n_along = matrix_along_by.rows();
  int n_by = matrix_along_by.cols();
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  for (int i_by = 0; i_by < n_by; i_by++) {
    int i = matrix_along_by(0, i_by);
    ans += dnorm(effectfree[i], Type(0), Type(1), true);
    for (int i_along = 1; i_along < n_along; i_along++) {
      int i_curr = matrix_along_by(i_along, i_by);
      int i_prev = matrix_along_by(i_along - 1, i_by);
      ans += dnorm(effectfree[i_curr], effectfree[i_prev], sd, true);
    }
  }
  return ans;
}

template <class Type>
Type logpost_rw2(vector<Type> effectfree,
		  vector<Type> hyper,
		  vector<Type> consts,
		  matrix<int> matrix_along_by) {
  Type scale = consts[0];
  Type log_sd = hyper[0];
  Type sd = exp(log_sd);
  int n_along = matrix_along_by.rows();
  int n_by = matrix_along_by.cols();
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  for (int i_by = 0; i_by < n_by; i_by++) {
    int i_0 = matrix_along_by(0, i_by);
    int i_1 = matrix_along_by(1, i_by);
    ans += dnorm(effectfree[i_0], Type(0), Type(1), true);
    ans += dnorm(effectfree[i_1], Type(0), Type(1), true);
    for (int i_along = 2; i_along < n_along; i_along++) {
      int i_2 = matrix_along_by(i_along, i_by);
      int i_1 = matrix_along_by(i_along - 1, i_by);
      int i_0 = matrix_along_by(i_along - 2, i_by);
      Type diff = effectfree[i_2] - 2 * effectfree[i_1] + effectfree[i_0];
      ans += dnorm(diff, Type(0), sd, true);
    }
  }
  return ans;
}

Type logpost_seasfix(vector<Type> seas,
		     int n_by) {
  int n = seas.size();
  int n_seas = n / n_by;
  Type ans = 0;
  for (int i_by = 0; i_by < n_by; i_by++) {
    for (int i_seas = 0; i_seas < n_seas; i_seas++) {
      int i = i_by * n_by + i_seas;
      ans += dnorm(seas[i], Type(0), Type(1));
    }
  }
  return ans;
}

Type logpost_seasfix(vector<Type> seas,
		     int n_by) {
  int n = seas.size();
  int n_seas = n / n_by;
  Type ans = 0;
  for (int i_by = 0; i_by < n_by; i_by++) {
    for (int i_seas = 0; i_seas < n_seas; i_seas++) {
      int i = i_by * n_by + i_seas;
      ans += dnorm(seas[i], Type(0), Type(1));
    }
  }
  return ans;
}


Type logpost_seasvary(vector<Type> seas,
		      matrix<int> matrix_along_by,
		      int n_seas,
		      Type sd_seas) {
  int n_along = matrix_along_by.rows();
  int n_by = matrix_along_by.cols();
  Type ans = 0;
  for (int i_by = 0; i_by < n_by; i_by++) {
    for (int i_along = 0; i_along < n_seas; i_along++) {
      int i = matrix_along_by(i_along, i_by);
      ans += dnorm(seas[i], Type(0), Type(1), true);
    }
    for (int i_along = n_seas; i_along < n_along; i_along++) {
      int i_curr = matrix_along_by(i_along, i_by);
      int i_prev = matrix_along_by(i_along - n_seas, i_by);
      ans += dnorm(seas[i_curr], seas[i_prev], sd_seas, true);
    }
  }
  return ans;
}
      
template <class Type>
Type logpost_rwseasfix(vector<Type> effectfree,
		       vector<Type> hyper,
		       vector<Type> hyperrand,
		       vector<Type> consts,
		       matrix<int> matrix_along_by) {
  Type scale = consts[0];
  Type scale_seas = consts[1];
  Type log_sd = hyper[0];
  vector<Type> seas = hyperrand;
  int n_along = matrix_along_by.rows();
  int n_by = matrix_along_by.cols();
  int n_seas = seas.size();
  Type sd = exp(log_sd);
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  ans += logpost_seasfix(seas, n_by);
  for (int i_by = 0; i_by < n_by; i_by++) {
    int i = matrix_along_by(0, i_by);
    ans += dnorm(effectfree[i], Type(0) + seas[i_by], Type(1), true);
    for (int i_along = 1; i_along < n_along; i_along++) {
      int i_curr = matrix_along_by(i_along, i_by);
      int i_prev = matrix_along_by(i_along - 1, i_by);
      int i_seas_curr = i_by * n_by + (i_along %% n_seas);
      int i_seas_prev = i_by * n_by + ((i_along - 1) %% n_seas);
      Type mean = effectfree[i_prev] + seas[i_seas_curr] - seas[i_seas_prev];
      ans += dnorm(effectfree[i_curr], mean, sd, true);
    }
  }
  return ans;
}


template <class Type>
Type logpost_rwseasvary(vector<Type> effectfree,
			vector<Type> hyper,
			vector<Type> hyperrand,
			vector<Type> consts,
			matrix<int> matrix_along_by) {
  Type scale = consts[0];
  Type scale_seas = consts[1];
  Type log_sd = hyper[0];
  Type log_sd_seas = hyper[1];
  vector<Type> seas = hyperrand;
  int n_along = matrix_along_by.rows();
  int n_by = matrix_along_by.cols();
  int n_seas = seas.size();
  Type sd = exp(log_sd);
  Type sd_seas = exp(log_sd);
  Type ans = 0;
  ans += logpost_seasvary(seas, matrix_along_by, n_seas, sd_seas);
  for (int i_by = 0; i_by < n_by; i_by++) {
    int i = matrix_along_by(0, i_by);
    ans += dnorm(effectfree[i], Type(0) + seas[0], Type(1), true);
    for (int i_along = 1; i_along < n_along; i_along++) {
      int i_curr = matrix_along_by(i_along, i_by);
      int i_prev = matrix_along_by(i_along - 1, i_by);
      Type mean = effectfree[i_prev] + seas[i_curr] - seas[i_prev];
      ans += dnorm(effectfree[i_curr], mean, sd, true);
    }
  }
  return ans;
}



template <class Type>
Type logpost_spline(vector<Type> effectfree,
   		    vector<Type> hyper,
		    vector<Type> consts,
		    matrix<int> matrix_along_by) {
  int n_by = matrix_along_by.cols();
  int n_free = effectfree.size();
  int n_along_free = n_free / n_by;
  matrix<int> matrix_along_by_free(n_along_free, n_by);
  int index = 0;
  for (int j = 0; j < n_by; j++)
    for (int i = 0; i < n_along_free; i++)
      matrix_along_by_free(i, j) = index++;
  return logpost_rw2(effectfree, hyper, consts, matrix_along_by_free);
}

template <class Type>
Type logpost_svd(vector<Type> effectfree,
		 vector<Type> consts,
		 matrix<int> matrix_along_by) {
  return dnorm(effectfree, Type(0), Type(1), true).sum();
}



// Equivalent of method dispatch ----------------------------------------------

template <class Type>
Type logpost_not_uses_hyper(vector<Type> effectfree,
			    vector<Type> consts,
			    matrix<int> matrix_along_by,
			    int i_prior) {
  Type ans = 0;
  switch(i_prior) {
  case 5:
    ans = logpost_normfixed(effectfree, consts, matrix_along_by);
    break;
  case 9:
    ans = logpost_svd(effectfree, consts, matrix_along_by);
    break;
  default:                                                                                          // # nocov
    error("Internal error: function 'logpost_not_uses_hyper' cannot handle i_prior = %d", i_prior); // # nocov
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
    ans = logpost_ar(effectfree, hyper, consts, matrix_along_by);
    break;
  case 4:
    ans = logpost_norm(effectfree, hyper, consts, matrix_along_by);
    break;
  case 6:
    ans = logpost_rw(effectfree, hyper, consts, matrix_along_by);
    break;
  case 7:
    ans = logpost_rw2(effectfree, hyper, consts, matrix_along_by);
    break;
  case 8:
    ans = logpost_spline(effectfree, hyper, consts, matrix_along_by);
    break;
  default:                                                                                      // # nocov
    error("Internal error: function 'logpost_uses_hyper' cannot handle i_prior = %d", i_prior); // # nocov
  }
  return ans;
}

template <class Type>
Type logpost_uses_hyperrand(vector<Type> effectfree,
			    vector<Type> hyper,
			    vector<Type> hyperrand,
			    vector<Type> consts,
			    matrix<int> matrix_along_by,
			    int i_prior) {
  Type ans = 0;
  switch(i_prior) {
  case 2:
    ans = logpost_lin(effectfree, hyper, hyperrand, consts, matrix_along_by);
    break;
  case 3:
    ans = logpost_linar(effectfree, hyper, hyperrand, consts, matrix_along_by);
    break;
  case 10:
    ans = logpost_rwseas(effectfree, hyper, hyperrand, consts, matrix_along_by);
    break;
  default:                                                                                          // # nocov
    error("Internal error: function 'logpost_uses_hyperrand' cannot handle i_prior = %d", i_prior); // # nocov
  }
  return ans;
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
        error("Internal error: not a sparse matrix"); // # nocov
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

template<class Type>
struct LIST_Type_t : vector<vector<Type> > {
  LIST_Type_t(SEXP x){
    (*this).resize(LENGTH(x));
    for (int i = 0; i < LENGTH(x); i++){
      SEXP v = VECTOR_ELT(x, i);
      (*this)(i) = asVector<Type>(v);
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
  DATA_STRUCT(offsets_effectfree_effect, LIST_Type_t);
  DATA_STRUCT(matrices_effect_outcome, LIST_SM_t);
  DATA_IVECTOR(i_prior);
  DATA_IVECTOR(uses_hyper);
  DATA_FACTOR(terms_hyper);
  DATA_IVECTOR(uses_hyperrand);
  DATA_FACTOR(terms_hyperrand);
  DATA_VECTOR(consts);
  DATA_FACTOR(terms_consts);
  DATA_STRUCT(matrices_along_by, LIST_M_t);
  DATA_SCALAR(mean_disp);

  PARAMETER_VECTOR(effectfree); 
  PARAMETER_VECTOR(hyper);
  PARAMETER_VECTOR(hyperrand);
  PARAMETER(log_disp);
  

  // intermediate quantities

  int n_outcome = outcome.size();
  int n_term = i_prior.size();
  vector<vector<Type> > effectfree_split = split(effectfree, terms_effectfree);
  vector<vector<Type> > hyper_split = split(hyper, terms_hyper);
  vector<vector<Type> > hyperrand_split = split(hyperrand, terms_hyperrand);
  vector<vector<Type> > consts_split = split(consts, terms_consts);
  int has_disp = mean_disp > 0;
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
      vector<Type> offset_term = offsets_effectfree_effect[i_term];
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
	if (uses_hyperrand[i_term]) { // if a prior uses hyperrand, then it uses hyper
	  vector<Type> hyperrand_term = hyperrand_split[i_term];
	  ans -= logpost_uses_hyperrand(effectfree_term,
					hyper_term,
					hyperrand_term,
					consts_term,
					matrix_along_by,
					i_prior_term);
	}
	else {
	  ans -= logpost_uses_hyper(effectfree_term,
				    hyper_term,
				    consts_term,
				    matrix_along_by,
				    i_prior_term);
	}
      } 
      else { // not uses hyper
	ans -= logpost_not_uses_hyper(effectfree_term,
				      consts_term,
				      matrix_along_by,
				      i_prior_term);
      }
    }
  }
  // contribution to log posterior from dispersion term
  if (has_disp) {
    Type rate_disp = 1 / mean_disp;
    ans -= dexp(disp, rate_disp, true);
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
	error("Internal error: invalid 'nm_distn' in logpost data"); // # nocov
      }
    }
  }

  return ans;
}
