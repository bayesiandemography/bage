
#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS
#include <TMB.hpp>
#include "init.h"

using namespace density;
using namespace Eigen;
using namespace tmbutils;


// Helper functions -----------------------------------------------------------

// Calculate alpha, given slope
template <class Type>
vector<Type> alpha_lin(vector<Type> effectfree,
		       vector<Type> hyperrandfree,
		       matrix<int> matrix_along_by_effectfree) {
  int n_along = matrix_along_by_effectfree.rows();
  int n_by = matrix_along_by_effectfree.cols();
  vector<Type> intercept = -0.5 * (n_along + 1) * hyperrandfree;
  vector<Type> ans = effectfree;
  for (int i_by = 0; i_by < n_by; i_by++) {
    for (int i_along = 0; i_along < n_along; i_along++) {
      int i = matrix_along_by_effectfree(i_along, i_by);
      ans[i] -= intercept[i_by] + (i_along + 1) * hyperrandfree[i_by];
    }
  }
  return ans;
}

// Calculate alpha, given free parameters and fixed seasonal effect.
// First seasonal effect is zero, and last equals sum of other effects.
template <class Type>
vector<Type> alpha_randomseasfix(vector<Type> effectfree,
				 vector<Type> seas,
				 vector<Type> consts,
				 matrix<int> matrix_along_by_effectfree) {
  int n_seas = CppAD::Integer(consts[0]);
  int n_along = matrix_along_by_effectfree.rows();
  int n_by = matrix_along_by_effectfree.cols();
  vector<Type> ans = effectfree;
  vector<Type> seas_sum(n_by);
  for (int i_by = 0; i_by < n_by; i_by++) {
    seas_sum[i_by] = 0;
    for (int i_along = 0; i_along < n_seas - 1; i_along++) {
      int i_seas = i_along + i_by * (n_seas - 1);
      seas_sum[i_by] += seas[i_seas];
    }
  }
  for (int i_by = 0; i_by < n_by; i_by++) {
    for (int i_along = 0; i_along < n_along; i_along++) {
      int i_alpha = matrix_along_by_effectfree(i_along, i_by);
      int index_seas = i_along % n_seas;
      bool is_last_seas = index_seas == n_seas - 1;
      if (!is_last_seas) {
	int i_seas = index_seas + i_by * (n_seas - 1);
	ans[i_alpha] -= seas[i_seas];
      }
      else {
	ans[i_along] += seas_sum[i_by];
      }
    }
  }
  return ans;
}

template <class Type>
vector<Type> alpha_randomseasvary(vector<Type> effectfree,
				  vector<Type> seas,
				  vector<Type> consts,
				  matrix<int> matrix_along_by_effectfree) {
  int n_seas = CppAD::Integer(consts[0]);
  int n_along_alpha = matrix_along_by_effectfree.rows();
  int n_by = matrix_along_by_effectfree.cols();
  int n_along_seas = seas.size() / n_by;
  vector<Type> ans = effectfree;
  for (int i_by = 0; i_by < n_by; i_by++) {
    int i_along_seas = 0;
    Type seas_sum = 0;
    for (int i_along_alpha = 0; i_along_alpha < n_along_alpha; i_along_alpha++) {
      int index_seas = i_along_alpha % n_seas;
      bool is_last_seas = index_seas == n_seas - 1;
      int i_alpha = matrix_along_by_effectfree(i_along_alpha, i_by);
      if (!is_last_seas) {
	int i_seas = i_along_seas + i_by * n_along_seas;
	i_along_seas++;
	ans[i_alpha] -= seas[i_seas];
	seas_sum += seas[i_seas];
      }
      else {
	ans[i_alpha] += seas_sum;
	seas_sum = 0;
      }
    }
  }
  return ans;
}

// use first value of random walk as first seasonal effect
template <class Type>
vector<Type> alpha_zeroseasfix(vector<Type> effectfree,
			       vector<Type> seas,
			       vector<Type> consts,
			       matrix<int> matrix_along_by_effectfree) {
  int n_seas = CppAD::Integer(consts[0]);
  int n_along = matrix_along_by_effectfree.rows();
  int n_by = matrix_along_by_effectfree.cols();
  vector<Type> ans = effectfree;
  vector<Type> effectfree_first(n_by);
  vector<Type> seas_sum(n_by);
  for (int i_by = 0; i_by < n_by; i_by++) {
    seas_sum[i_by] = 0;
    for (int i_along = 0; i_along < n_seas - 2; i_along++) {
      int i_seas = i_along + i_by * (n_seas - 2);
      seas_sum[i_by] += seas[i_seas];
    }
  }
  for (int i_by = 0; i_by < n_by; i_by++) {
    int i_effectfree_first = matrix_along_by_effectfree(0, i_by);
    effectfree_first[i_by] = effectfree[i_effectfree_first];
  }
  for (int i_by = 0; i_by < n_by; i_by++) {
    for (int i_along = 0; i_along < n_along; i_along++) {
      int index_seas = i_along % n_seas;
      bool is_first_seas = index_seas == 0;
      bool is_last_seas = index_seas == n_seas - 1;
      int i_alpha = matrix_along_by_effectfree(i_along, i_by);
      if (!is_first_seas && !is_last_seas) {
	int i_seas = index_seas - 1 + i_by * (n_seas - 2);
	ans[i_alpha] -= seas[i_seas];
      }
      if (is_last_seas) {
	ans[i_alpha] += seas_sum[i_by] + n_seas * effectfree_first[i_by];
      }
    }
  }
  return ans;
}

template <class Type>
vector<Type> alpha_zeroseasvary(vector<Type> effectfree,
				vector<Type> seas,
				vector<Type> consts,
				matrix<int> matrix_along_by_effectfree) {
  int n_seas = CppAD::Integer(consts[0]);
  int n_along_alpha = matrix_along_by_effectfree.rows();
  int n_by = matrix_along_by_effectfree.cols();
  int n_along_seas = seas.size() / n_by;
  vector<Type> ans = effectfree;
  for (int i_by = 0; i_by < n_by; i_by++) {
    int i_along_seas = 0;
    Type seas_sum = 0;
    int i_effectfree_first = matrix_along_by_effectfree(0, i_by);
    Type effectfree_first = effectfree[i_effectfree_first];
    for (int i_along_alpha = 0; i_along_alpha < n_along_alpha; i_along_alpha++) {
      int index_seas = i_along_alpha % n_seas;
      bool is_first_element = i_along_alpha == 0;
      bool is_last_seas = index_seas == n_seas - 1;
      int i_alpha = matrix_along_by_effectfree(i_along_alpha, i_by);
      if (!is_first_element && !is_last_seas) {
	int i_seas = i_along_seas + i_by * n_along_seas;
	i_along_seas++;
	ans[i_alpha] -= seas[i_seas];
	seas_sum += seas[i_seas];
      }
      if (is_last_seas) {
	ans[i_alpha] += seas_sum + n_seas * effectfree_first;
	seas_sum = 0;
      }
    }
  }
  return ans;
}

template <class Type>
Type logpost_ar_inner(vector<Type> effectfree,
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
  ans += dbeta(coef_raw, shape1, shape2, true).sum() +
    log(coef_raw).sum() +
    log(1 - coef_raw).sum();
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
Type logpost_seasfix(vector<Type> seas,
		     vector<Type> consts) {
  Type sd_seas = consts[1];
  return dnorm(seas, Type(0), sd_seas, true).sum();
}

template <class Type>
Type logpost_seasvary(vector<Type> seas,
		      vector<Type> hyper,
		      vector<Type> consts,
		      matrix<int> matrix_along_by_effectfree) {
  int n_seas = CppAD::Integer(consts[0]);
  Type scale_innov = consts[1];
  Type sd_init = consts[2];
  Type log_sd_innov = hyper[0];
  Type sd_innov = exp(log_sd_innov);
  int n_by = matrix_along_by_effectfree.cols();
  int n_along_seas = seas.size() / n_by;
  Type ans = 0;
  ans += dnorm(sd_innov, Type(0), scale_innov, true) + log_sd_innov;
  for (int i_by = 0; i_by < n_by; i_by++) {
    for (int i_along_seas = 0; i_along_seas < n_along_seas; i_along_seas++) {
      int i_seas = i_along_seas + i_by * n_along_seas;
      int is_initial_season = i_along_seas < n_seas - 2;
      int is_second_element_first_season = i_along_seas == n_seas - 2;
      if (is_initial_season) {
	ans += dnorm(seas[i_seas], Type(0), sd_init, true);
      }
      else if (is_second_element_first_season) {
	ans += dnorm(seas[i_seas], Type(0), sd_innov, true);
      }
      else {
	int i_seas_prev = i_seas - n_seas + 1;
	ans += dnorm(seas[i_seas], seas[i_seas_prev], sd_innov, true);
      }
    }
  }
  return ans;
}

template <class Type>
Type logpost_slope(vector<Type> slope,
		   vector<Type> consts) {
  Type mean_slope = consts[0];
  Type sd_slope = consts[1];
  return dnorm(slope, mean_slope, sd_slope, true).sum();
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
  return logpost_ar_inner(effectfree, hyper, consts, matrix_along_by_effectfree);
}

template <class Type>
Type logpost_lin(vector<Type> effectfree,
		 vector<Type> hyper,
		 vector<Type> hyperrandfree, // slope
		 vector<Type> consts,
		 matrix<int> matrix_along_by_effectfree) {
  Type scale = consts[0];
  Type mean_slope = consts[1];
  Type sd_slope = consts[2];
  int n_along = matrix_along_by_effectfree.rows();
  int n_by = matrix_along_by_effectfree.cols();
  Type log_sd = hyper[0];
  vector<Type> intercept = -0.5 * (n_along + 1.0) * hyperrandfree;
  Type sd = exp(log_sd);
  Type ans = 0;
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  ans += dnorm(hyperrandfree, mean_slope, sd_slope, true).sum();
  for (int i_by = 0; i_by < n_by; i_by++) {
    for (int i_along = 0; i_along < n_along; i_along++) {
      int i = matrix_along_by_effectfree(i_along, i_by);
      ans += dnorm(effectfree[i], intercept[i_by] + (i_along + 1) * hyperrandfree[i_by], sd, true);
    }
  }
  return ans;
}

template <class Type>
Type logpost_linar(vector<Type> effectfree,
		   vector<Type> hyper,
		   vector<Type> hyperrandfree, // slope
		   vector<Type> consts,
		   matrix<int> matrix_along_by_effectfree) {
  vector<Type> consts_slope = consts.head(2);
  vector<Type> consts_ar = consts.tail(5);
  vector<Type> ar = alpha_lin(effectfree, hyperrandfree, matrix_along_by_effectfree);
  Type ans = 0;
  ans += logpost_slope(hyperrandfree, consts_slope);
  ans += logpost_ar_inner(ar, hyper, consts_ar, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_linex(vector<Type> effectfree, // slope
		   vector<Type> consts,
		   matrix<int> matrix_along_by_effectfree) {
  Type mean_slope = consts[0];
  Type sd_slope = consts[1];
  return dnorm(effectfree, mean_slope, sd_slope, true).sum();
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
Type logpost_rwrandom(vector<Type> rw,
		      vector<Type> hyper,
		      vector<Type> consts,
		      matrix<int> matrix_along_by) {
  Type scale_innov = consts[0];
  Type sd_init = consts[1];
  Type log_sd_innov = hyper[0];
  Type sd_innov = exp(log_sd_innov);
  int n_along = matrix_along_by.rows();
  int n_by = matrix_along_by.cols();
  Type ans = 0;
  ans += dnorm(sd_innov, Type(0), scale_innov, true) + log_sd_innov;
  for (int i_by = 0; i_by < n_by; i_by++) {
    int i = matrix_along_by(0, i_by);
    ans += dnorm(rw[i], Type(0), sd_init, true);
    for (int i_along = 1; i_along < n_along; i_along++) {
      int i_curr = matrix_along_by(i_along, i_by);
      int i_prev = matrix_along_by(i_along - 1, i_by);
      ans += dnorm(rw[i_curr], rw[i_prev], sd_innov, true);
    }
  }
  return ans;
}

template <class Type>
Type logpost_rwrandomseasfix(vector<Type> effectfree,
			     vector<Type> hyper,
			     vector<Type> hyperrandfree, // seasonal effect
			     vector<Type> consts,
			     matrix<int> matrix_along_by_effectfree) {
  vector<Type> consts_seas = consts.head(2); // n_seas, sd_seas
  vector<Type> consts_rw(2);
  consts_rw[0] = consts[2]; // scale
  consts_rw[1] = consts[1]; // sd_seas 
  vector<Type> rw = alpha_randomseasfix(effectfree,
					hyperrandfree,
					consts_seas,
					matrix_along_by_effectfree);
  Type ans = 0;
  ans += logpost_seasfix(hyperrandfree, consts_seas);
  ans += logpost_rwrandom(rw, hyper, consts_rw, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_rwrandomseasvary(vector<Type> effectfree,
			      vector<Type> hyper,
			      vector<Type> hyperrandfree, // seasonal effect
			      vector<Type> consts,
			      matrix<int> matrix_along_by_effectfree) {
  vector<Type> consts_seas = consts.head(3); // n_seas, scale_seas, sd_seas
  vector<Type> consts_rw(2);
  consts_rw[0] = consts[3]; // scale
  consts_rw[1] = consts[2]; // sd_seas, used for sd
  vector<Type> hyper_seas = hyper.head(1);   // log_sd_seas
  vector<Type> hyper_rw = hyper.tail(1);     // log_sd
  vector<Type> rw = alpha_randomseasvary(effectfree,
					 hyperrandfree,
					 consts_seas,
					 matrix_along_by_effectfree);
  Type ans = 0;
  ans += logpost_seasvary(hyperrandfree, hyper_seas, consts_seas, matrix_along_by_effectfree);
  ans += logpost_rwrandom(rw, hyper_rw, consts_rw, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_rwzero(vector<Type> rw,
		    vector<Type> hyper,
		    vector<Type> consts,
		    matrix<int> matrix_along_by) {
  Type scale_innov = consts[0];
  Type log_sd_innov = hyper[0];
  Type sd_innov = exp(log_sd_innov);
  int n_along = matrix_along_by.rows();
  int n_by = matrix_along_by.cols();
  Type ans = 0;
  ans += dnorm(sd_innov, Type(0), scale_innov, true) + log_sd_innov;
  for (int i_by = 0; i_by < n_by; i_by++) {
    int i = matrix_along_by(0, i_by);
    ans += dnorm(rw[i], Type(0), sd_innov, true);
    for (int i_along = 1; i_along < n_along; i_along++) {
      int i_curr = matrix_along_by(i_along, i_by);
      int i_prev = matrix_along_by(i_along - 1, i_by);
      ans += dnorm(rw[i_curr], rw[i_prev], sd_innov, true);
    }
  }
  return ans;
}

template <class Type>
Type logpost_rwzeroseasfix(vector<Type> effectfree,
			   vector<Type> hyper,
			   vector<Type> hyperrandfree, // seasonal effect
			   vector<Type> consts,
			   matrix<int> matrix_along_by_effectfree) {
  vector<Type> consts_seas = consts.head(2); // n_seas, sd_seas
  vector<Type> consts_rw(2);
  consts_rw[0] = consts[2]; // scale
  consts_rw[1] = consts[1]; // sd_seas 
  vector<Type> rw = alpha_zeroseasfix(effectfree,
				      hyperrandfree,
				      consts_seas,
				      matrix_along_by_effectfree);
  Type ans = 0;
  ans += logpost_seasfix(hyperrandfree, consts_seas);
  ans += logpost_rwrandom(rw, hyper, consts_rw, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_rwzeroseasvary(vector<Type> effectfree,
			    vector<Type> hyper,
			    vector<Type> hyperrandfree, // seasonal effect
			    vector<Type> consts,
			    matrix<int> matrix_along_by_effectfree) {
  vector<Type> consts_seas = consts.head(3); // n_seas, scale_seas, sd_seas
  vector<Type> consts_rw(2);
  consts_rw[0] = consts[3]; // scale
  consts_rw[1] = consts[2]; // sd_seas, used for sd
  vector<Type> hyper_seas = hyper.head(1);   // log_sd_seas
  vector<Type> hyper_rw = hyper.tail(1);     // log_sd
  vector<Type> rw = alpha_zeroseasvary(effectfree,
				       hyperrandfree,
				       consts_seas,
				       matrix_along_by_effectfree);
  Type ans = 0;
  ans += logpost_seasvary(hyperrandfree, hyper_seas, consts_seas, matrix_along_by_effectfree);
  ans += logpost_rwrandom(rw, hyper_rw, consts_rw, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_rw2infant(vector<Type> effectfree,
		       vector<Type> hyper,
		       vector<Type> consts,
		       matrix<int> matrix_along_by_effectfree) {
  Type scale_innov = consts[0];
  Type sd_slope = consts[1];
  Type log_sd_innov = hyper[0];
  Type sd_innov = exp(log_sd_innov);
  int n_along = matrix_along_by_effectfree.rows();
  int n_by = matrix_along_by_effectfree.cols();
  Type ans = 0;
  ans += dnorm(sd_innov, Type(0), scale_innov, true) + log_sd_innov;
  for (int i_by = 0; i_by < n_by; i_by++) {
    int i_0 = matrix_along_by_effectfree(0, i_by);
    ans += dnorm(effectfree[i_0], Type(0), Type(1), true);
    int i_1 = matrix_along_by_effectfree(1, i_by);
    ans += dnorm(effectfree[i_1], Type(0), sd_slope, true);
    int i_2 = matrix_along_by_effectfree(2, i_by);
    Type diff = effectfree[i_2] - 2 * effectfree[i_1];
    ans += dnorm(diff, Type(0), sd_innov, true);
    for (int i_along = 3; i_along < n_along; i_along++) {
      int i_2 = matrix_along_by_effectfree(i_along, i_by);
      int i_1 = matrix_along_by_effectfree(i_along - 1, i_by);
      int i_0 = matrix_along_by_effectfree(i_along - 2, i_by);
      Type diff = effectfree[i_2] - 2 * effectfree[i_1] + effectfree[i_0];
      ans += dnorm(diff, Type(0), sd_innov, true);
    }
  }
  return ans;
}

template <class Type>
Type logpost_rw2random(vector<Type> rw,
		       vector<Type> hyper,
		       vector<Type> consts,
		       matrix<int> matrix_along_by) {
  Type scale_innov = consts[0];
  Type sd_init = consts[1];
  Type sd_slope = consts[2];
  Type log_sd_innov = hyper[0];
  Type sd_innov = exp(log_sd_innov);
  int n_along = matrix_along_by.rows();
  int n_by = matrix_along_by.cols();
  Type ans = 0;
  ans += dnorm(sd_innov, Type(0), scale_innov, true) + log_sd_innov;
  for (int i_by = 0; i_by < n_by; i_by++) {
    int i_0 = matrix_along_by(0, i_by);
    int i_1 = matrix_along_by(1, i_by);
    ans += dnorm(rw[i_0], Type(0), sd_init, true);
    Type diff = rw[i_1] - rw[i_0];
    ans += dnorm(diff, Type(0), sd_slope, true);
    for (int i_along = 2; i_along < n_along; i_along++) {
      int i_2 = matrix_along_by(i_along, i_by);
      int i_1 = matrix_along_by(i_along - 1, i_by);
      int i_0 = matrix_along_by(i_along - 2, i_by);
      Type diff = rw[i_2] - 2 * rw[i_1] + rw[i_0];
      ans += dnorm(diff, Type(0), sd_innov, true);
    }
  }
  return ans;
}

template <class Type>
Type logpost_rw2randomseasfix(vector<Type> effectfree,
			      vector<Type> hyper,
			      vector<Type> hyperrandfree, // seasonal effect
			      vector<Type> consts,
			      matrix<int> matrix_along_by_effectfree) {
  vector<Type> consts_seas = consts.head(2); // n_seas, sd_seas
  vector<Type> consts_rw = consts.tail(3); // scale, sd, sd_slope
  vector<Type> rw = alpha_randomseasfix(effectfree,
					hyperrandfree,
					consts_seas,
					matrix_along_by_effectfree);
  Type ans = 0;
  ans += logpost_seasfix(hyperrandfree, consts_seas);
  ans += logpost_rw2random(rw, hyper, consts_rw, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_rw2randomseasvary(vector<Type> effectfree,
			       vector<Type> hyper,
			       vector<Type> hyperrandfree, // seasonal effect
			       vector<Type> consts,
			       matrix<int> matrix_along_by_effectfree) {
  vector<Type> consts_seas = consts.head(3); // n_seas, scale_seas, sd_seas
  vector<Type> consts_rw = consts.tail(3); // scale, sd, sd_slope
  vector<Type> hyper_seas = hyper.head(1); // log_sd_seas
  vector<Type> hyper_rw = hyper.tail(1); // log_sd
  vector<Type> rw = alpha_randomseasvary(effectfree,
					 hyperrandfree,
					 consts_seas,
					 matrix_along_by_effectfree);
  Type ans = 0;
  ans += logpost_seasvary(hyperrandfree, hyper_seas, consts_seas, matrix_along_by_effectfree);
  ans += logpost_rw2random(rw, hyper_rw, consts_rw, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_rw2zero(vector<Type> rw,
		     vector<Type> hyper,
		     vector<Type> consts,
		     matrix<int> matrix_along_by) {
  Type scale_innov = consts[0];
  Type sd_slope = consts[1];
  Type log_sd_innov = hyper[0];
  Type sd_innov = exp(log_sd_innov);
  int n_along = matrix_along_by.rows();
  int n_by = matrix_along_by.cols();
  Type ans = 0;
  ans += dnorm(sd_innov, Type(0), scale_innov, true) + log_sd_innov;
  for (int i_by = 0; i_by < n_by; i_by++) {
    int i_0 = matrix_along_by(0, i_by);
    int i_1 = matrix_along_by(1, i_by);
    ans += dnorm(rw[i_0], Type(0), sd_slope, true);
    Type diff = rw[i_1] - 2 * rw[i_0];
    ans += dnorm(diff, Type(0), sd_innov, true);
    for (int i_along = 2; i_along < n_along; i_along++) {
      int i_2 = matrix_along_by(i_along, i_by);
      int i_1 = matrix_along_by(i_along - 1, i_by);
      int i_0 = matrix_along_by(i_along - 2, i_by);
      Type diff = rw[i_2] - 2 * rw[i_1] + rw[i_0];
      ans += dnorm(diff, Type(0), sd_innov, true);
    }
  }
  return ans;
}

template <class Type>
Type logpost_rw2zeroseasfix(vector<Type> effectfree,
			    vector<Type> hyper,
			    vector<Type> hyperrandfree, // seasonal effect
			    vector<Type> consts,
			    matrix<int> matrix_along_by_effectfree) {
  vector<Type> consts_seas = consts.head(2); // n_seas, sd_seas
  vector<Type> consts_rw(3);
  consts_rw[0] = consts[2]; // scale
  consts_rw[1] = consts[1]; // sd_seas, used for sd
  consts_rw[2] = consts[3]; // sd_slope
  vector<Type> rw = alpha_zeroseasfix(effectfree,
				      hyperrandfree,
				      consts_seas,
				      matrix_along_by_effectfree);
  Type ans = 0;
  ans += logpost_seasfix(hyperrandfree, consts_seas);
  ans += logpost_rw2random(rw, hyper, consts_rw, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_rw2zeroseasvary(vector<Type> effectfree,
			     vector<Type> hyper,
			     vector<Type> hyperrandfree, // seasonal effect
			     vector<Type> consts,
			     matrix<int> matrix_along_by_effectfree) {
  vector<Type> consts_seas = consts.head(3); // n_seas, scale_seas, sd_seas
  vector<Type> consts_rw(3);
  consts_rw[0] = consts[3]; // scale
  consts_rw[1] = consts[2]; // sd_seas, used for sd
  consts_rw[2] = consts[4]; // sd_slope
  vector<Type> hyper_seas = hyper.head(1); // log_sd_seas
  vector<Type> hyper_rw = hyper.tail(1); // log_sd
  vector<Type> rw = alpha_zeroseasvary(effectfree,
				       hyperrandfree,
				       consts_seas,
				       matrix_along_by_effectfree);
  Type ans = 0;
  ans += logpost_seasvary(hyperrandfree, hyper_seas, consts_seas, matrix_along_by_effectfree);
  ans += logpost_rw2random(rw, hyper_rw, consts_rw, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_spline(vector<Type> effectfree,
   		    vector<Type> hyper,
		    vector<Type> consts,
		    matrix<int> matrix_along_by_effectfree) {
  return logpost_rw2random(effectfree, hyper, consts, matrix_along_by_effectfree);
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
  return logpost_ar_inner(effectfree, hyper, consts, matrix_along_by_effectfree);
}

template <class Type>
Type logpost_svd_rwrandom(vector<Type> effectfree,
			  vector<Type> hyper,
			  vector<Type> consts,
			  matrix<int> matrix_along_by_effectfree) {
  return logpost_rwrandom(effectfree, hyper, consts, matrix_along_by_effectfree);
}

template <class Type>
Type logpost_svd_rwzero(vector<Type> effectfree,
			vector<Type> hyper,
			vector<Type> consts,
			matrix<int> matrix_along_by_effectfree) {
  return logpost_rwzero(effectfree, hyper, consts, matrix_along_by_effectfree);
}

template <class Type>
Type logpost_svd_rw2random(vector<Type> effectfree,
			   vector<Type> hyper,
			   vector<Type> consts,
			   matrix<int> matrix_along_by_effectfree) {
  return logpost_rw2random(effectfree, hyper, consts, matrix_along_by_effectfree);
}

template <class Type>
Type logpost_svd_rw2zero(vector<Type> effectfree,
			 vector<Type> hyper,
			 vector<Type> consts,
			 matrix<int> matrix_along_by_effectfree) {
  return logpost_rw2zero(effectfree, hyper, consts, matrix_along_by_effectfree);
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
  case 17:
    ans = logpost_linex(effectfree, consts, matrix_along_by_effectfree);
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
    ans = logpost_rwzero(effectfree, hyper, consts, matrix_along_by_effectfree);
    break;
  case 7:
    ans = logpost_rw2zero(effectfree, hyper, consts, matrix_along_by_effectfree);
    break;
  case 8:
    ans = logpost_spline(effectfree, hyper, consts, matrix_along_by_effectfree);
    break;
  case 14:
    ans = logpost_svd_ar(effectfree, hyper, consts, matrix_along_by_effectfree);
    break;
  case 15:
    ans = logpost_svd_rwzero(effectfree, hyper, consts, matrix_along_by_effectfree);
    break;
  case 16:
    ans = logpost_svd_rw2zero(effectfree, hyper, consts, matrix_along_by_effectfree);
    break;
  case 18:
    ans = logpost_rw2infant(effectfree, hyper, consts, matrix_along_by_effectfree);
    break;
  case 19:
    ans = logpost_rwrandom(effectfree, hyper, consts, matrix_along_by_effectfree);
    break;
  case 22:
    ans = logpost_rw2random(effectfree, hyper, consts, matrix_along_by_effectfree);
    break;
  case 25:
    ans = logpost_svd_rwrandom(effectfree, hyper, consts, matrix_along_by_effectfree);
    break;
  case 26:
    ans = logpost_svd_rw2random(effectfree, hyper, consts, matrix_along_by_effectfree);
    break;
  default:                                                                                      // # nocov
    error("Internal error: function 'logpost_uses_hyper' cannot handle i_prior = %d", i_prior); // # nocov
  }
  return ans;
}

template <class Type>
Type logpost_uses_hyperrandfree(vector<Type> effectfree,
				vector<Type> hyper,
				vector<Type> hyperrandfree,
				vector<Type> consts,
				matrix<int> matrix_along_by_effectfree,
				int i_prior) {
  Type ans = 0;
  switch(i_prior) {
  case 2:
    ans = logpost_lin(effectfree, hyper, hyperrandfree, consts, matrix_along_by_effectfree);
    break;
  case 3:
    ans = logpost_linar(effectfree, hyper, hyperrandfree, consts, matrix_along_by_effectfree);
    break;
  case 10:
    ans = logpost_rwzeroseasfix(effectfree, hyper, hyperrandfree, consts, matrix_along_by_effectfree);
    break;
  case 11:
    ans = logpost_rwzeroseasvary(effectfree, hyper, hyperrandfree, consts, matrix_along_by_effectfree);
    break;
  case 12:
    ans = logpost_rw2zeroseasfix(effectfree, hyper, hyperrandfree, consts, matrix_along_by_effectfree);
    break;
  case 13:
    ans = logpost_rw2zeroseasvary(effectfree, hyper, hyperrandfree, consts,
				  matrix_along_by_effectfree);
    break;
  case 20:
    ans = logpost_rwrandomseasfix(effectfree, hyper, hyperrandfree, consts,
				  matrix_along_by_effectfree);
    break;
  case 21:
    ans = logpost_rwrandomseasvary(effectfree, hyper, hyperrandfree, consts,
				   matrix_along_by_effectfree);
    break;
  case 23:
    ans = logpost_rw2randomseasfix(effectfree, hyper, hyperrandfree, consts,
				   matrix_along_by_effectfree);
    break;
  case 24:
    ans = logpost_rw2randomseasvary(effectfree, hyper, hyperrandfree, consts,
				    matrix_along_by_effectfree);
    break;
  default:                                                                                          // # nocov
    error("Internal error: function 'logpost_uses_hyperrandfree' cannot handle i_prior = %d", i_prior); // # nocov
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
  DATA_IVECTOR(uses_hyperrandfree);
  DATA_FACTOR(terms_hyperrandfree);
  DATA_VECTOR(consts);
  DATA_FACTOR(terms_consts);
  DATA_STRUCT(matrices_along_by_effectfree, LIST_M_t);
  DATA_SCALAR(mean_disp);

  PARAMETER_VECTOR(effectfree); 
  PARAMETER_VECTOR(hyper);
  PARAMETER_VECTOR(hyperrandfree);
  PARAMETER(log_disp);
  

  // intermediate quantities

  int n_outcome = outcome.size();
  int n_term = i_prior.size();
  vector<vector<Type> > effectfree_split = split(effectfree, terms_effectfree);
  vector<vector<Type> > hyper_split = split(hyper, terms_hyper);
  vector<vector<Type> > hyperrandfree_split = split(hyperrandfree, terms_hyperrandfree);
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
	if (uses_hyperrandfree[i_term]) { // if a prior uses hyperrandfree, then it uses hyper
	  vector<Type> hyperrandfree_term = hyperrandfree_split[i_term];
	  ans -= logpost_uses_hyperrandfree(effectfree_term,
					    hyper_term,
					    hyperrandfree_term,
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
    Type out = outcome[i_outcome];
    Type lin = linpred[i_outcome];
    Type off = offset[i_outcome];
    if (has_disp)
      ans -= loglik_uses_disp(out, lin, off, disp, i_lik);
    else
      ans -= loglik_not_uses_disp(out, lin, off, i_lik);
  }
  return ans;
}
