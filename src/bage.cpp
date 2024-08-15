
#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS
#include <TMB.hpp>
#include "init.h"

using namespace density;
using namespace Eigen;
using namespace tmbutils;


// Helper functions -----------------------------------------------------------

template <class Type>
vector<Type> alpha_seasfix(vector<Type> effectfree,
			   vector<Type> seas,
			   int n_season,
			   matrix<int> matrix_along_by_effectfree) {
  int n_along = matrix_along_by_effectfree.rows();
  int n_by = matrix_along_by_effectfree.cols();
  vector<Type> ans = effectfree;
  for (int i_by = 0; i_by < n_by; i_by++) {
    for (int i_along = 0; i_along < n_along; i_along++) {
      int i_alpha = matrix_along_by_effectfree(i_along, i_by);
      int i_seas = i_along % n_season + i_by * n_season;
      ans[i_alpha] -= seas[i_seas];
    }
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


template <class Type>
Type logpost_seasvary(vector<Type> seas,
		      int n_season,
		      Type scale_seas,
		      Type log_sd_seas,
		      matrix<int> matrix_along_by_effectfree) {
  int n_along = matrix_along_by_effectfree.rows();
  int n_by = matrix_along_by_effectfree.cols();
  Type sd_seas = exp(log_sd_seas);
  Type ans = 0;
  ans += dnorm(sd_seas, Type(0), scale_seas, true) + log_sd_seas;
  for (int i_by = 0; i_by < n_by; i_by++) {
    for (int i_along = 0; i_along < n_season; i_along++) {
      int i = matrix_along_by_effectfree(i_along, i_by);
      ans += dnorm(seas[i], Type(0), Type(1), true);
    }
    for (int i_along = n_season; i_along < n_along; i_along++) {
      int i_curr = matrix_along_by_effectfree(i_along, i_by);
      int i_prev = matrix_along_by_effectfree(i_along - n_season, i_by);
      ans += dnorm(seas[i_curr], seas[i_prev], sd_seas, true);
    }
  }
  return ans;
}


// "Methods" for 'logpost' function  ------------------------------------------

// Assume inputs all valid (checking done in R).
// Note that the 'Known' prior does not have
// an associated 'logpost' method.

template <class Type>
Type logpost_ar(vector<Type> effectfree,
		vector<Type> hyper,
		vector<Type> consts,
		matrix<int> matrix_along_by_effectfree) {
  Type shape1 = consts[0];
  Type shape2 = consts[1];
  Type min = consts[2];
  Type max = consts[3];
  Type scale = consts[4];
  int n_along = matrix_along_by_effectfree.rows();
  int n_by = matrix_along_by_effectfree.cols();
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
      int i = matrix_along_by_effectfree(i_along, i_by);
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
		 matrix<int> matrix_along_by_effectfree) {
  Type scale = consts[0];
  Type sd_slope = consts[1];
  int n_along = matrix_along_by_effectfree.rows();
  int n_by = matrix_along_by_effectfree.cols();
  Type log_sd = hyper[0];
  vector<Type> intercept = hyperrand.head(n_by);
  vector<Type> slope = hyperrand.tail(n_by);
  Type sd = exp(log_sd);
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  ans += dnorm(intercept, Type(0), Type(1), true).sum();
  ans += dnorm(slope, Type(0), sd_slope, true).sum();
  for (int i_by = 0; i_by < n_by; i_by++) {
    for (int i_along = 0; i_along < n_along; i_along++) {
      int i = matrix_along_by_effectfree(i_along, i_by);
      ans += dnorm(effectfree[i], intercept[i_by] + (i_along + 1) * slope[i_by], sd, true);
    }
  }
  return ans;
}

template <class Type>
Type logpost_linar(vector<Type> effectfree,
		   vector<Type> hyper,
		   vector<Type> hyperrand,
		   vector<Type> consts,
		   matrix<int> matrix_along_by_effectfree) {
  Type scale = consts[0];
  Type sd_slope = consts[1];
  Type shape1 = consts[2];
  Type shape2 = consts[3];
  Type min = consts[4];
  Type max = consts[5];
  Type log_sd = hyper[0];
  int n_coef = hyper.size() - 1;
  int n_along = matrix_along_by_effectfree.rows();
  int n_by = matrix_along_by_effectfree.cols();
  vector<Type> logit_coef = hyper.segment(1, n_coef);
  vector<Type> intercept = hyperrand.head(n_by);
  vector<Type> slope = hyperrand.tail(n_by);
  Type sd = exp(log_sd);
  vector<Type> coef_raw = exp(logit_coef) / (1 + exp(logit_coef));
  vector<Type> coef = (max - min) * coef_raw + min;
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  ans += dnorm(intercept, Type(0), Type(1), true).sum();
  ans += dnorm(slope, Type(0), sd_slope, true).sum();
  Type radius = sqrt((coef_raw * coef_raw).sum());
  ans += dbeta(radius, shape1, shape2, true)
    + log(coef_raw).sum()
    + log(1 - coef_raw).sum();
  for (int i_by = 0; i_by < n_by; i_by++) {
    vector<Type> err(n_along);
    for (int i_along = 0; i_along < n_along; i_along++) {
      int i = matrix_along_by_effectfree(i_along, i_by);
      err[i_along] = effectfree[i] - intercept[i_by] - (i_along + 1) * slope[i_by];
    }
    ans -= SCALE(ARk(coef), sd)(err); // ARk returns neg log-lik
  }
  return ans;
}

template <class Type>
Type logpost_norm(vector<Type> effectfree,
		  vector<Type> hyper,
		  vector<Type> consts,
		  matrix<int> matrix_along_by_effectfree) {
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
		       matrix<int> matrix_along_by_effectfree) {
  Type sd = consts[0];
  Type ans = 0;
  ans += dnorm(effectfree, Type(0), sd, true).sum();
  return ans;
}

template <class Type>
Type logpost_rw(vector<Type> effectfree,
		vector<Type> hyper,
		vector<Type> consts,
		matrix<int> matrix_along_by_effectfree) {
  Type scale = consts[0];
  Type log_sd = hyper[0];
  Type sd = exp(log_sd);
  int n_along = matrix_along_by_effectfree.rows();
  int n_by = matrix_along_by_effectfree.cols();
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  for (int i_by = 0; i_by < n_by; i_by++) {
    int i = matrix_along_by_effectfree(0, i_by);
    ans += dnorm(effectfree[i], Type(0), Type(1), true);
    for (int i_along = 1; i_along < n_along; i_along++) {
      int i_curr = matrix_along_by_effectfree(i_along, i_by);
      int i_prev = matrix_along_by_effectfree(i_along - 1, i_by);
      ans += dnorm(effectfree[i_curr], effectfree[i_prev], sd, true);
    }
  }
  return ans;
}

template <class Type>
Type logpost_rwseasfix(vector<Type> effectfree,
		       vector<Type> hyper,
		       vector<Type> hyperrand, // seasonal effect
		       vector<Type> consts,
		       matrix<int> matrix_along_by_effectfree) {
  int n_season = CppAD::Integer(consts[0]);
  int n_consts = consts.size();
  vector<Type> consts_alpha = consts.segment(1, n_consts - 1);
  vector<Type> alpha = alpha_seasfix(effectfree, hyperrand, n_season, matrix_along_by_effectfree);
  Type ans = 0;
  ans += dnorm(hyperrand, Type(0), Type(1), true).sum();
  ans += logpost_rw(alpha, hyper, consts_alpha, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_rwseasvary(vector<Type> effectfree,
			vector<Type> hyper,
			vector<Type> hyperrand, // seasonal effect
			vector<Type> consts,
			matrix<int> matrix_along_by_effectfree) {
  vector<Type> alpha = effectfree - hyperrand;
  int n_season = CppAD::Integer(consts[0]);
  Type scale_seas = consts[1];
  Type log_sd_seas = hyper[0];
  int n_hyper = hyper.size();
  int n_consts = consts.size();
  vector<Type> hyper_alpha = hyper.segment(1, n_hyper - 1);
  vector<Type> consts_alpha = consts.segment(2, n_consts - 2);
  Type ans = 0;
  ans += logpost_seasvary(hyperrand, n_season, scale_seas, log_sd_seas, matrix_along_by_effectfree);
  ans += logpost_rw(alpha, hyper_alpha, consts_alpha, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_rw2(vector<Type> effectfree,
		  vector<Type> hyper,
		  vector<Type> consts,
		  matrix<int> matrix_along_by_effectfree) {
  Type scale = consts[0];
  Type log_sd = hyper[0];
  Type sd = exp(log_sd);
  int n_along = matrix_along_by_effectfree.rows();
  int n_by = matrix_along_by_effectfree.cols();
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  for (int i_by = 0; i_by < n_by; i_by++) {
    int i_0 = matrix_along_by_effectfree(0, i_by);
    int i_1 = matrix_along_by_effectfree(1, i_by);
    ans += dnorm(effectfree[i_0], Type(0), Type(1), true);
    ans += dnorm(effectfree[i_1], Type(0), Type(1), true);
    for (int i_along = 2; i_along < n_along; i_along++) {
      int i_2 = matrix_along_by_effectfree(i_along, i_by);
      int i_1 = matrix_along_by_effectfree(i_along - 1, i_by);
      int i_0 = matrix_along_by_effectfree(i_along - 2, i_by);
      Type diff = effectfree[i_2] - 2 * effectfree[i_1] + effectfree[i_0];
      ans += dnorm(diff, Type(0), sd, true);
    }
  }
  return ans;
}

template <class Type>
Type logpost_rw2seasfix(vector<Type> effectfree,
			vector<Type> hyper,
			vector<Type> hyperrand, // seasonal effect
			vector<Type> consts,
			matrix<int> matrix_along_by_effectfree) {
  int n_season = CppAD::Integer(consts[0]);
  int n_consts = consts.size();
  vector<Type> consts_alpha = consts.segment(1, n_consts - 1);
  vector<Type> alpha = alpha_seasfix(effectfree, hyperrand, n_season, matrix_along_by_effectfree);
  Type ans = 0;
  ans += dnorm(hyperrand, Type(0), Type(1), true).sum();
  ans += logpost_rw2(alpha, hyper, consts_alpha, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_rw2seasvary(vector<Type> effectfree,
			 vector<Type> hyper,
			 vector<Type> hyperrand, // seasonal effect
			 vector<Type> consts,
			 matrix<int> matrix_along_by_effectfree) {
  vector<Type> alpha = effectfree - hyperrand;
  int n_season = CppAD::Integer(consts[0]);
  Type scale_seas = consts[1];
  Type log_sd_seas = hyper[0];
  int n_hyper = hyper.size();
  int n_consts = consts.size();
  vector<Type> hyper_alpha = hyper.segment(1, n_hyper - 1);
  vector<Type> consts_alpha = consts.segment(2, n_consts - 2);
  Type ans = 0;
  ans += logpost_seasvary(hyperrand, n_season, scale_seas, log_sd_seas, matrix_along_by_effectfree);
  ans += logpost_rw2(alpha, hyper_alpha, consts_alpha, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_spline(vector<Type> effectfree,
   		    vector<Type> hyper,
		    vector<Type> consts,
		    matrix<int> matrix_along_by_effectfree) {
  return logpost_rw2(effectfree, hyper, consts, matrix_along_by_effectfree);
}

template <class Type>
Type logpost_svd(vector<Type> effectfree,
		 vector<Type> consts,
		 matrix<int> matrix_along_by_effectfree) {
  return dnorm(effectfree, Type(0), Type(1), true).sum();
}

template <class Type>
Type logpost_svd_ar(vector<Type> effectfree,
		    vector<Type> hyper,
		    vector<Type> consts,
		    matrix<int> matrix_along_by_effectfree) {
  return logpost_ar(effectfree, hyper, consts, matrix_along_by_effectfree);
}

template <class Type>
Type logpost_svd_rw(vector<Type> effectfree,
		    vector<Type> hyper,
		    vector<Type> consts,
		    matrix<int> matrix_along_by_effectfree) {
  return logpost_rw(effectfree, hyper, consts, matrix_along_by_effectfree);
}

template <class Type>
Type logpost_svd_rw2(vector<Type> effectfree,
		     vector<Type> hyper,
		     vector<Type> consts,
		     matrix<int> matrix_along_by_effectfree) {
  return logpost_rw2(effectfree, hyper, consts, matrix_along_by_effectfree);
}


// Equivalent of method dispatch for logpost ----------------------------------

template <class Type>
Type logpost_not_uses_hyper(vector<Type> effectfree,
			    vector<Type> consts,
			    matrix<int> matrix_along_by_effectfree,
			    int i_prior) {
  Type ans = 0;
  switch(i_prior) {
  case 5:
    ans = logpost_normfixed(effectfree, consts, matrix_along_by_effectfree);
    break;
  case 9:
    ans = logpost_svd(effectfree, consts, matrix_along_by_effectfree);
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
			matrix<int> matrix_along_by_effectfree,
			int i_prior) {
  Type ans = 0;
  switch(i_prior) {
  case 1:
    ans = logpost_ar(effectfree, hyper, consts, matrix_along_by_effectfree);
    break;
  case 4:
    ans = logpost_norm(effectfree, hyper, consts, matrix_along_by_effectfree);
    break;
  case 6:
    ans = logpost_rw(effectfree, hyper, consts, matrix_along_by_effectfree);
    break;
  case 7:
    ans = logpost_rw2(effectfree, hyper, consts, matrix_along_by_effectfree);
    break;
  case 8:
    ans = logpost_spline(effectfree, hyper, consts, matrix_along_by_effectfree);
    break;
  case 14:
    ans = logpost_svd_ar(effectfree, hyper, consts, matrix_along_by_effectfree);
    break;
  case 15:
    ans = logpost_svd_rw(effectfree, hyper, consts, matrix_along_by_effectfree);
    break;
  case 16:
    ans = logpost_svd_rw2(effectfree, hyper, consts, matrix_along_by_effectfree);
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
			    matrix<int> matrix_along_by_effectfree,
			    int i_prior) {
  Type ans = 0;
  switch(i_prior) {
  case 2:
    ans = logpost_lin(effectfree, hyper, hyperrand, consts, matrix_along_by_effectfree);
    break;
  case 3:
    ans = logpost_linar(effectfree, hyper, hyperrand, consts, matrix_along_by_effectfree);
    break;
  case 10:
    ans = logpost_rwseasfix(effectfree, hyper, hyperrand, consts, matrix_along_by_effectfree);
    break;
  case 11:
    ans = logpost_rwseasvary(effectfree, hyper, hyperrand, consts, matrix_along_by_effectfree);
    break;
  case 12:
    ans = logpost_rw2seasfix(effectfree, hyper, hyperrand, consts, matrix_along_by_effectfree);
    break;
  case 13:
    ans = logpost_rw2seasvary(effectfree, hyper, hyperrand, consts, matrix_along_by_effectfree);
    break;
  default:                                                                                          // # nocov
    error("Internal error: function 'logpost_uses_hyperrand' cannot handle i_prior = %d", i_prior); // # nocov
  }
  return ans;
}


// 'Methods' for loglik -------------------------------------------------------

template <class Type>
Type loglik_binom_not_uses_disp(Type outcome,
				Type linpred,
				Type offset) {
  return dbinom_robust(outcome, offset, linpred, true);
}

template <class Type>
Type loglik_binom_not_uses_disp_rr3(Type outcome,
				    Type linpred,
				    Type offset) {
  const Type log_one_third = -log(3);
  const Type log_two_thirds = log(2) - log(3);
  Type ans = 0;
  if (outcome >= 2)
    ans += log_one_third + dbinom_robust(outcome - 2, offset, linpred, true);
  if (outcome >= 1)
    ans = logspace_add(ans, log_two_thirds + dbinom_robust(outcome - 1, offset, linpred, true));
  ans = logspace_add(ans, dbinom_robust(outcome, offset, linpred, true));
  ans = logspace_add(ans, log_two_thirds + dbinom_robust(outcome + 1, offset, linpred, true));
  ans = logspace_add(ans, log_one_third + dbinom_robust(outcome + 2, offset, linpred, true));
  return ans;
}

template <class Type>
Type loglik_binom_uses_disp(Type outcome,
			    Type linpred,
			    Type offset,
			    Type disp) {
  return logpost_betabinom(outcome, offset, linpred, disp);
}

template <class Type>
Type loglik_binom_uses_disp_rr3(Type outcome,
				Type linpred,
				Type offset,
				Type disp) {
  const Type log_one_third = -log(3);
  const Type log_two_thirds = log(2) - log(3);
  Type ans = 0;
  if (outcome >= 2)
    ans += log_one_third + logpost_betabinom(outcome - 2, offset, linpred, disp);
  if (outcome >= 1)
    ans = logspace_add(ans, log_two_thirds + logpost_betabinom(outcome - 1, offset, linpred, disp));
  ans = logspace_add(ans, logpost_betabinom(outcome, offset, linpred, disp));
  ans = logspace_add(ans, log_two_thirds + logpost_betabinom(outcome + 1, offset, linpred, disp));
  ans = logspace_add(ans, log_one_third + logpost_betabinom(outcome + 2, offset, linpred, disp));
  return ans;
}

template <class Type>
Type loglik_norm(Type outcome,
		 Type linpred,
		 Type offset,
		 Type disp) {
  Type sd = disp / sqrt(offset);
  Type ans = dnorm(outcome, linpred, sd, true);
  return ans;
}

template <class Type>
Type loglik_pois_not_uses_disp(Type outcome,
			       Type linpred,
			       Type offset) {
  Type rate = exp(linpred) * offset;
  Type ans = dpois(outcome, rate, true);
  return ans;
}

template <class Type>
Type loglik_pois_not_uses_disp_rr3(Type outcome,
				   Type linpred,
				   Type offset) {
  Type rate = exp(linpred) * offset;
  const Type log_one_third = -log(3);
  const Type log_two_thirds = log(2) - log(3);
  Type ans = 0;
  if (outcome >= 2)
    ans += log_one_third + dpois(outcome - 2, rate, true);
  if (outcome >= 1)
    ans = logspace_add(ans, log_two_thirds + dpois(outcome - 1, rate, true));
  ans = logspace_add(ans, dpois(outcome, rate, true));
  ans = logspace_add(ans, log_two_thirds + dpois(outcome + 1, rate, true));
  ans = logspace_add(ans, log_one_third + dpois(outcome + 2, rate, true));
  return ans;
}

template <class Type>
Type loglik_pois_uses_disp(Type outcome,
			   Type linpred,
			   Type offset,
			   Type disp) {
  Type rate = exp(linpred) * offset;
  Type size = 1 / disp;
  Type prob = size / (rate + size);
  Type ans = dnbinom(outcome, size, prob, true);
  return ans;
}

template <class Type>
Type loglik_pois_uses_disp_rr3(Type outcome,
			       Type linpred,
			       Type offset,
			       Type disp) {
  Type rate = exp(linpred) * offset;
  Type size = 1 / disp;
  Type prob = size / (rate + size);
  const Type log_one_third = -log(3);
  const Type log_two_thirds = log(2) - log(3);
  Type ans = 0;
  if (outcome >= 2)
    ans += log_one_third + dnbinom(outcome - 2, size, prob, true);
  if (outcome >= 1)
    ans = logspace_add(ans, log_two_thirds + dnbinom(outcome - 1, size, prob, true));
  ans = logspace_add(ans, dnbinom(outcome, size, prob, true));
  ans = logspace_add(ans, log_two_thirds + dnbinom(outcome + 1, size, prob, true));
  ans = logspace_add(ans, log_one_third + dnbinom(outcome + 2, size, prob, true));
  return ans;
}


// Equivalent of method dispatch for loglik -----------------------------------

template <class Type>
Type loglik_not_uses_disp(Type outcome,
			  Type linpred,
			  Type offset,
			  int i_lik) {
  Type ans = 0;
  switch(i_lik) {
  case 101:
    ans = loglik_binom_not_uses_disp(outcome, linpred, offset);
    break;
  case 102:
    ans = loglik_binom_not_uses_disp_rr3(outcome, linpred, offset);
    break;
  case 301:
    ans = loglik_pois_not_uses_disp(outcome, linpred, offset);
    break;
  case 302:
    ans = loglik_pois_not_uses_disp_rr3(outcome, linpred, offset);
    break;
  default:                                                                             // # nocov
    error("Internal error: function 'loglik_not_uses_disp' cannot handle i_lik = %d",  // # nocov
	  i_lik);                                                                      // # nocov
  }
  return ans;
}

template <class Type>
Type loglik_uses_disp(Type outcome,
		      Type linpred,
		      Type offset,
		      Type disp,
		      int i_lik) {
  Type ans = 0;
  switch(i_lik) {
  case 103:
    ans = loglik_binom_uses_disp(outcome, linpred, offset, disp);
    break;
  case 104:
    ans = loglik_binom_uses_disp_rr3(outcome, linpred, offset, disp);
    break;
  case 201:
    ans = loglik_norm(outcome, linpred, offset, disp);
    break;
  case 303:
    ans = loglik_pois_uses_disp(outcome, linpred, offset, disp);
    break;
  case 304:
    ans = loglik_pois_uses_disp_rr3(outcome, linpred, offset, disp);
    break;
  default:                                                                         // # nocov
    error("Internal error: function 'loglik_uses_disp' cannot handle i_lik = %d",  // # nocov
	  i_lik);                                                                  // # nocov
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

  DATA_INTEGER(i_lik);
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
  DATA_STRUCT(matrices_along_by_effectfree, LIST_M_t);
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

  vector<Type> linpred(n_outcome);
  linpred.fill(0);
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
    linpred = linpred + matrix_effect_outcome * effect_term;
  }

  // negative log posterior
  
  Type ans = 0;

  // contribution to log posterior from priors
  for (int i_term = 0; i_term < n_term; i_term++) {
    int i_prior_term = i_prior[i_term];
    if (i_prior_term > 0) { // i_prior_term == 0 when prior is "Known"
      vector<Type> effectfree_term = effectfree_split[i_term];
      vector<Type> consts_term = consts_split[i_term];
      matrix<int> matrix_along_by_effectfree = matrices_along_by_effectfree[i_term];
      if (uses_hyper[i_term]) {
	vector<Type> hyper_term = hyper_split[i_term];
	if (uses_hyperrand[i_term]) { // if a prior uses hyperrand, then it uses hyper
	  vector<Type> hyperrand_term = hyperrand_split[i_term];
	  ans -= logpost_uses_hyperrand(effectfree_term,
					hyper_term,
					hyperrand_term,
					consts_term,
					matrix_along_by_effectfree,
					i_prior_term);
	}
	else {
	  ans -= logpost_uses_hyper(effectfree_term,
				    hyper_term,
				    consts_term,
				    matrix_along_by_effectfree,
				    i_prior_term);
	}
      } 
      else { // not uses hyper
	ans -= logpost_not_uses_hyper(effectfree_term,
				      consts_term,
				      matrix_along_by_effectfree,
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
      Type out = outcome[i_outcome];
      Type lin = linpred[i_outcome];
      Type off = offset[i_outcome];
      if (has_disp)
	ans -= loglik_uses_disp(out, lin, off, disp, i_lik);
      else
	ans -= loglik_not_uses_disp(out, lin, off, i_lik);
    }
  }
  return ans;
}
