
#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS
#include <TMB.hpp>
#include "init.h"

using namespace density;
using namespace Eigen;
using namespace tmbutils;


// Types of priors ------------------------------------------------------------

template <class Type>
Type logpost_not_uses_hyper(vector<Type> effectfree,
			    vector<Type> consts,
			    matrix<int> matrix_along_by,
			    int i_prior);

template <class Type>
Type logpost_uses_hyper(vector<Type> effectfree,
			vector<Type> hyper,
			vector<Type> consts,
			matrix<int> matrix_along_by,
			int i_prior);

template <class Type>
Type logpost_uses_hyperrand(vector<Type> effectfree,
			    vector<Type> hyper,
			    vector<Type> hyperrand,
			    vector<Type> consts,
			    matrix<int> matrix_along_by,
			    int i_prior);


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


// "methods" for 'logpost' function  ------------------------------------------

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
  int n_coef = hyper.size() - 1;
  vector<Type> logit_coef = hyper.head(n_coef);
  Type log_sd = hyper[n_coef];
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
Type logpost_compose(vector<Type> effectfree,
		     vector<Type> hyper,
		     vector<Type> hyperrand,
		     vector<Type> consts,
		     matrix<int> matrix_along_by,
		     vector<int> indices_priors) {
  constexpr int n_position = 7;
  int n_comp = indices_priors.size() / n_position;
  int n_effect = effectfree.size();
  Type ans = 0;
  vector<Type> effectfree_comp(n_effect);       
  vector<Type> effectfree_total(n_effect); 
  for (int i_effect = 0; i_effect < n_effect; i_effect++)
    effectfree_total[i_effect] = 0;
  for (int i_comp = 0; i_comp < n_comp; i_comp++) {
    int offset = i_comp * n_position;
    int hyper_start = indices_priors[offset];
    int hyper_length = indices_priors[offset + 1];
    int hyperrand_start = indices_priors[offset + 2];
    int hyperrand_length = indices_priors[offset + 3];
    int consts_start = indices_priors[offset + 4];
    int consts_length = indices_priors[offset + 5];
    int i_prior_comp = indices_priors[offset + 6];
    // extract info
    bool uses_hyper = hyper_length > 0;
    bool is_comp_last = i_comp == n_comp - 1;
    bool uses_hyperrand;
    if (is_comp_last) {
      effectfree_comp = effectfree - effectfree_total;
      uses_hyperrand = hyperrand_length > 0;
    }
    else {
      effectfree_comp = hyperrand.segment(hyperrand_start, n_effect);
      effectfree_total += effectfree_comp;
      uses_hyperrand = hyperrand_length > n_effect;
    }
    vector<Type> consts_comp = consts.segment(consts_start, consts_length);
    // calculate log posterior density
    if (uses_hyper) {
      vector<Type> hyper_comp = hyper.segment(hyper_start, hyper_length);
      if (uses_hyperrand) {
	vector<Type> hyperrand_comp = hyperrand.segment(hyperrand_start, hyperrand_length);
	ans += logpost_uses_hyperrand(effectfree_comp,
				      hyper_comp,
				      hyperrand_comp,
				      consts_comp,
				      matrix_along_by,
				      i_prior_comp);
      }
      else {
	ans += logpost_uses_hyper(effectfree_comp,
				  hyper_comp,
				  consts_comp,
				  matrix_along_by,
				  i_prior_comp);
      }
    }
    else {
      error("Internal error: 'compose' prior with no hyper-parameters."); // # nocov
    }
  }
  return ans;
}

template <class Type>
Type logpost_ear(vector<Type> effectfree,
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
  ans += dbeta(coef_raw, shape1, shape2, true).sum()
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
Type logpost_elin(vector<Type> effectfree,
		  vector<Type> hyper,
		  vector<Type> hyperrand,
		  vector<Type> consts,
		  matrix<int> matrix_along_by) {
  Type scale = consts[0];
  Type sd_slope = consts[1];
  Type mscale = consts[2];
  int n_along = matrix_along_by.rows();
  int n_by = matrix_along_by.cols();
  Type slope = hyper[0];
  Type log_sd = hyper[1];
  Type log_msd = hyper[2];
  vector<Type> mslope = hyperrand;
  Type sd = exp(log_sd);
  Type msd = exp(log_msd);
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
Type logpost_erw(vector<Type> effectfree,
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
Type logpost_eseas(vector<Type> effectfree,
		   vector<Type> hyper,
		   vector<Type> consts,
		   matrix<int> matrix_along_by) {
  Type scale = consts[0];
  int n_season = consts.size(); // size of 'consts' used to record 'n_season'
  int n_along = matrix_along_by.rows();
  int n_by = matrix_along_by.cols();
  Type log_sd = hyper[0];
  Type sd = exp(log_sd);
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  for (int i_by = 0; i_by < n_by; i_by++) {
    for (int i_along = 0; i_along < n_season; i_along++) {
      int i = matrix_along_by(i_along, i_by);
      ans += dnorm(effectfree[i], Type(0), Type(1), true);
    }
    for (int i_along = n_season; i_along < n_along; i_along++) {
      int i_curr = matrix_along_by(i_along, i_by);
      int i_prev = matrix_along_by(i_along - n_season, i_by);
      ans += dnorm(effectfree[i_curr], effectfree[i_prev], sd, true);
    }
  }
  return ans;
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
Type logpost_seas(vector<Type> effectfree,
		  vector<Type> hyper,
		  vector<Type> consts,
		  matrix<int> matrix_along_by) {
  Type scale = consts[0];
  int n_season = consts.size(); // size of 'consts' used to record 'n_season'
  int n_effect = effectfree.size();
  Type log_sd = hyper[0];
  Type sd = exp(log_sd);
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  for (int i = 0; i < n_season; i++)
    ans += dnorm(effectfree[i], Type(0), Type(1), true);
  for (int i = n_season; i < n_effect; i++)
    ans += dnorm(effectfree[i], effectfree[i-n_season], sd, true);
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


// Equivalent of method dispatch ----------------------------------------------

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
  case 10:
    ans = logpost_seas(effectfree, hyper, consts, matrix_along_by);
    break;
  case 11:
    ans = logpost_eseas(effectfree, hyper, consts, matrix_along_by);
    break;
  case 12:
    ans = logpost_ear(effectfree, hyper, consts, matrix_along_by);
    break;
  case 13:
    ans = logpost_erw(effectfree, hyper, consts, matrix_along_by);
    break;
  default:
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
  case 9:
    ans = logpost_elin(effectfree, hyper, hyperrand, consts, matrix_along_by);
    break;
  default:
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
  DATA_IVECTOR(uses_indices_priors);
  DATA_IVECTOR(indices_priors);
  DATA_FACTOR(terms_indices_priors);
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
  vector<vector<int> > indices_priors_split = split(indices_priors, terms_indices_priors);
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
	  if (uses_indices_priors[i_term]) {
	    vector<int> indices_priors_term = indices_priors_split[i_term];
	    ans -= logpost_compose(effectfree_term,
				   hyper_term,
				   hyperrand_term,
				   consts_term,
				   matrix_along_by,
				   indices_priors_term);
	  }
	  else {
	    ans -= logpost_uses_hyperrand(effectfree_term,
					  hyper_term,
					  hyperrand_term,
					  consts_term,
					  matrix_along_by,
					  i_prior_term);
	  }
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
