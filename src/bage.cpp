#include <TMB.hpp>
#include "init.h"

using namespace density;
using namespace Eigen;
using namespace tmbutils;



// Constants ------------------------------------------------------------------

constexpr double LOG_ONE_THIRD  = -1.0986122886681098;
constexpr double LOG_TWO_THIRDS = -0.4054651081081644;


// Alias for dynamic matrix ---------------------------------------------------

template<class Type>
using MatrixD = Eigen::Matrix<Type, Eigen::Dynamic, Eigen::Dynamic>;

// List objects to hold matrices ----------------------------------------------

// Modified from code at https://github.com/kaskr/adcomp/issues/96

template<class Type>
struct LIST_SM_t : vector<SparseMatrix<Type> > {
  LIST_SM_t(SEXP x){
    (*this).resize(LENGTH(x));
    for (int i = 0; i < LENGTH(x); i++){
      SEXP sm = VECTOR_ELT(x, i);
      if(!isValidSparseMatrix(sm))
        Rf_error("Internal error: not a sparse matrix"); // # nocov
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


// Helper functions -----------------------------------------------------------

// Calculate alpha, given slope
template <class Type>
vector<Type> alpha_lin(const vector<Type>& effectfree,
		       const vector<Type>& hyperrandfree,
		       const matrix<int>& matrix_along_by_effectfree) {
  const int n_along = matrix_along_by_effectfree.rows();
  const int n_by = matrix_along_by_effectfree.cols();
  const vector<Type> intercept = -0.5 * (n_along + 1) * hyperrandfree;
  vector<Type> ans = effectfree;
  for (int i_by = 0; i_by < n_by; i_by++) {
    for (int i_along = 0; i_along < n_along; i_along++) {
      const int i = matrix_along_by_effectfree(i_along, i_by);
      ans[i] -= intercept[i_by] + (i_along + 1) * hyperrandfree[i_by];
    }
  }
  return ans;
}

// Calculate alpha, given free parameters and fixed seasonal effect.
// First seasonal effect is zero, and last equals sum of other effects.
template <class Type>
vector<Type> alpha_randomseasfix(const vector<Type>& effectfree,
				 const vector<Type>& seas,
				 const vector<Type>& consts,
				 const matrix<int>& matrix_along_by_effectfree) {
  const int n_seas = CppAD::Integer(consts[0]);
  const int n_along = matrix_along_by_effectfree.rows();
  const int n_by = matrix_along_by_effectfree.cols();
  vector<Type> ans = effectfree;
  vector<Type> seas_sum(n_by);
  for (int i_by = 0; i_by < n_by; i_by++) {
    seas_sum[i_by] = 0;
    for (int i_along = 0; i_along < n_seas - 1; i_along++) {
      const int i_seas = i_along + i_by * (n_seas - 1);
      seas_sum[i_by] += seas[i_seas];
    }
  }
  for (int i_by = 0; i_by < n_by; i_by++) {
    for (int i_along = 0; i_along < n_along; i_along++) {
      const int i_alpha = matrix_along_by_effectfree(i_along, i_by);
      const int index_seas = i_along % n_seas;
      const bool is_last_seas = index_seas == n_seas - 1;
      if (!is_last_seas) {
	const int i_seas = index_seas + i_by * (n_seas - 1);
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
vector<Type> alpha_randomseasvary(const vector<Type>& effectfree,
				  const vector<Type>& seas,
				  const vector<Type>& consts,
				  const matrix<int>& matrix_along_by_effectfree) {
  const int n_seas = CppAD::Integer(consts[0]);
  const int n_along_alpha = matrix_along_by_effectfree.rows();
  const int n_by = matrix_along_by_effectfree.cols();
  const int n_along_seas = seas.size() / n_by;
  vector<Type> ans = effectfree;
  for (int i_by = 0; i_by < n_by; i_by++) {
    int i_along_seas = 0;
    Type seas_sum = 0;
    for (int i_along_alpha = 0; i_along_alpha < n_along_alpha; i_along_alpha++) {
      const int index_seas = i_along_alpha % n_seas;
      const bool is_last_seas = index_seas == n_seas - 1;
      const int i_alpha = matrix_along_by_effectfree(i_along_alpha, i_by);
      if (!is_last_seas) {
	const int i_seas = i_along_seas + i_by * n_along_seas;
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
vector<Type> alpha_zeroseasfix(const vector<Type>& effectfree,
			       const vector<Type>& seas,
			       const vector<Type>& consts,
			       const matrix<int>& matrix_along_by_effectfree) {
  const int n_seas = CppAD::Integer(consts[0]);
  const int n_along = matrix_along_by_effectfree.rows();
  const int n_by = matrix_along_by_effectfree.cols();
  vector<Type> ans = effectfree;
  vector<Type> effectfree_first(n_by);
  vector<Type> seas_sum(n_by);
  for (int i_by = 0; i_by < n_by; i_by++) {
    seas_sum[i_by] = 0;
    for (int i_along = 0; i_along < n_seas - 2; i_along++) {
      const int i_seas = i_along + i_by * (n_seas - 2);
      seas_sum[i_by] += seas[i_seas];
    }
  }
  for (int i_by = 0; i_by < n_by; i_by++) {
    const int i_effectfree_first = matrix_along_by_effectfree(0, i_by);
    effectfree_first[i_by] = effectfree[i_effectfree_first];
  }
  for (int i_by = 0; i_by < n_by; i_by++) {
    for (int i_along = 0; i_along < n_along; i_along++) {
      const int index_seas = i_along % n_seas;
      const bool is_first_seas = index_seas == 0;
      const bool is_last_seas = index_seas == n_seas - 1;
      const int i_alpha = matrix_along_by_effectfree(i_along, i_by);
      if (!is_first_seas && !is_last_seas) {
	const int i_seas = index_seas - 1 + i_by * (n_seas - 2);
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
vector<Type> alpha_zeroseasvary(const vector<Type>& effectfree,
				const vector<Type>& seas,
				const vector<Type>& consts,
				const matrix<int>& matrix_along_by_effectfree) {
  const int n_seas = CppAD::Integer(consts[0]);
  const int n_along_alpha = matrix_along_by_effectfree.rows();
  const int n_by = matrix_along_by_effectfree.cols();
  const int n_along_seas = seas.size() / n_by;
  vector<Type> ans = effectfree;
  for (int i_by = 0; i_by < n_by; i_by++) {
    int i_along_seas = 0;
    Type seas_sum = 0;
    const int i_effectfree_first = matrix_along_by_effectfree(0, i_by);
    const Type effectfree_first = effectfree[i_effectfree_first];
    for (int i_along_alpha = 0; i_along_alpha < n_along_alpha; i_along_alpha++) {
      const int index_seas = i_along_alpha % n_seas;
      const bool is_first_element = i_along_alpha == 0;
      const bool is_last_seas = index_seas == n_seas - 1;
      int i_alpha = matrix_along_by_effectfree(i_along_alpha, i_by);
      if (!is_first_element && !is_last_seas) {
	const int i_seas = i_along_seas + i_by * n_along_seas;
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
Type dnbinom_mu(Type x,
		Type size,
		Type mu,
		bool give_log) {
  const Type prob = Type(1) / (Type(1) + mu / size);  // stable form
  return dnbinom(x, size, prob, give_log);
}

template <class Type>
Type log_dbetabinom(Type x,
		    Type size,
		    Type mu,
		    Type disp) {
  Type alpha = mu / disp;
  Type beta = (Type(1) - mu) / disp;
  Type log_num = lgamma(x + alpha) + lgamma(size - x + beta) - lgamma(size + alpha + beta);
  Type log_den = lgamma(alpha) + lgamma(beta) - lgamma(alpha + beta);
  return log_num - log_den;
}

template <class Type>
Type log_dbetabinom_rr3(Type x,
			Type size,
			Type mu,
			Type disp) {
  Type ans = log_dbetabinom(x, size, mu, disp);
  if (asDouble(x) >= 2.0)
    ans = logspace_add(ans,
		      LOG_ONE_THIRD
		      + log_dbetabinom(x - Type(2), size, mu, disp));
  if (asDouble(x) >= 1.0)
    ans = logspace_add(ans,
		       LOG_TWO_THIRDS
		       + log_dbetabinom(x - Type(1), size, mu, disp));
  ans = logspace_add(ans,
		     LOG_TWO_THIRDS
		     + log_dbetabinom(x + Type(1), size, mu, disp));
  ans = logspace_add(ans,
		     LOG_ONE_THIRD
		     + log_dbetabinom(x + Type(2), size, mu, disp));
  return ans;
}

template <class Type>
Type log_dbinom_rr3(Type x,
		    Type size,
		    Type prob) {
  Type ans = dbinom(x, size, prob, true);
  if (asDouble(x) >= 2.0)
    ans += logspace_add(ans,
			LOG_ONE_THIRD
			+ dbinom(x - Type(2), size, prob, true));
  if (asDouble(x) >= 1.0)
    ans = logspace_add(ans,
		       LOG_TWO_THIRDS
		       + dbinom(x - Type(1), size, prob, true));
  ans = logspace_add(ans,
		     LOG_TWO_THIRDS
		     + dbinom(x + Type(1), size, prob, true));
  ans = logspace_add(ans,
		     LOG_ONE_THIRD
		     + dbinom(x + Type(2), size, prob, true));
  return ans;
}

template <class Type>
Type log_dbinom_robust_rr3(Type x,
			   Type size,
			   Type logit_p) {
  Type ans = dbinom_robust(x, size, logit_p, true);
  if (asDouble(x) >= 2.0)
    ans = logspace_add(ans,
		       LOG_ONE_THIRD
		       + dbinom_robust(x - Type(2), size, logit_p, true));
  if (asDouble(x) >= 1.0)
    ans = logspace_add(ans,
		       LOG_TWO_THIRDS
		       + dbinom_robust(x - Type(1), size, logit_p, true));
  ans = logspace_add(ans,
		     LOG_TWO_THIRDS
		     + dbinom_robust(x + Type(1), size, logit_p, true));
  ans = logspace_add(ans,
		     LOG_ONE_THIRD
		     + dbinom_robust(x + Type(2), size, logit_p, true));
  return ans;
}

template <class Type>
Type log_dnbinom_rr3(Type x,
		     Type size,
		     Type prob) {
  Type ans = dnbinom(x, size, prob, true);
  if (asDouble(x) >= 2.0)
    ans += logspace_add(ans,
			LOG_ONE_THIRD
			+ dnbinom(x - Type(2), size, prob, true));
  if (asDouble(x) >= 1.0)
    ans = logspace_add(ans,
		       LOG_TWO_THIRDS
		       + dnbinom(x - Type(1), size, prob, true));
  ans = logspace_add(ans,
		     LOG_TWO_THIRDS
		     + dnbinom(x + Type(1), size, prob, true));
  ans = logspace_add(ans,
		     LOG_ONE_THIRD
		     + dnbinom(x + Type(2), size, prob, true));
  return ans;
}

template <class Type>
Type log_dnbinom_mu_rr3(Type x,
			Type size,
			Type mu) {
  Type ans = dnbinom_mu(x, size, mu, true);
  if (asDouble(x) >= 2.0)
    ans += logspace_add(ans,
			LOG_ONE_THIRD
			+ dnbinom_mu(x - Type(2), size, mu, true));
  if (asDouble(x) >= 1.0)
    ans = logspace_add(ans,
		       LOG_TWO_THIRDS
		       + dnbinom_mu(x - Type(1), size, mu, true));
  ans = logspace_add(ans,
		     LOG_TWO_THIRDS
		     + dnbinom_mu(x + Type(1), size, mu, true));
  ans = logspace_add(ans,
		     LOG_ONE_THIRD
		     + dnbinom_mu(x + Type(2), size, mu, true));
  return ans;
}

template <class Type>
Type log_dpois_rr3(Type x, Type rate) {
  Type ans = dpois(x, rate, true);
  if (asDouble(x) >= 2.0)
    ans = logspace_add(ans,
		       LOG_ONE_THIRD
		       + dpois(x - Type(2), rate, true));
  if (asDouble(x) >= 1.0)
    ans = logspace_add(ans,
		       LOG_TWO_THIRDS
		       + dpois(x - Type(1), rate, true));
  ans = logspace_add(ans,
		     LOG_TWO_THIRDS
		     + dpois(x + Type(1), rate, true));
  ans = logspace_add(ans,
		     LOG_ONE_THIRD
		     + dpois(x + Type(2), rate, true));
  return ans;
}



template<class Type>
Type log_dskellam_exact(Type x, Type mu1, Type mu2) {
  Type nu = fabs(x);
  Type v = Type(2.0) * sqrt(mu1 * mu2);
  return -(mu1 + mu2)
    + Type(0.5) * x * (log(mu1) - log(mu2))
    + log(besselI(v, nu));
}

// uses saddle point approximation
template<class Type>
Type log_dskellam_approx(Type x, Type mu1, Type mu2) {
  Type s = (x + sqrt(x * x + 4.0 * mu1 * mu2)) / (2.0 * mu1); // positive solution to K'(s) = x, s = e^t
  Type t = log(s);
  Type s_inv = Type(1.0) / s;
  Type K = mu1 * (s - Type(1.0)) + mu2 * (s_inv - Type(1.0));
  Type K2 = mu1 * s + mu2 * s_inv;
  return -Type(0.5) * log(Type(2.0) * Type(M_PI))
    - Type(0.5) * log(K2)
    + K - t * x;
}

template<class Type>
Type log_dskellam(Type x, Type mu1, Type mu2) {
  const Type thresh_small_mu = 5.0;
  const Type thresh_small_x = 30.0;
  const double mu1d = asDouble(mu1);
  const double mu2d = asDouble(mu2);
  const double xd = asDouble(x);
  // cases where mu1, mu2 <= 0
  const bool is_mu1_nonpos = !(mu1d > 0.0);
  if (is_mu1_nonpos) {
    if (xd > 0.0) // # nocov
      return -std::numeric_limits<Type>::infinity(); // # nocov
    else 
      return dpois(-x, mu2, true); // # nocov
  }
  const bool is_mu2_nonpos = !(mu2d > 0.0);
  if (is_mu2_nonpos) {
    if (xd < 0.0) // # nocov
      return -std::numeric_limits<Type>::infinity(); // # nocov
    else
      return dpois(x, mu1, true); // # nocov
  }
  // case where mu1, mu2 > 0
  bool mu_small = mu1d + mu2d < thresh_small_mu;
  bool x_small = std::abs(xd) < thresh_small_x;
  bool use_exact = mu_small && x_small;
  if (use_exact)
    return log_dskellam_exact(x, mu1, mu2);
  else
    return log_dskellam_approx(x, mu1, mu2);
}

template <class Type>
Type log_dskellam_rr3(Type x, Type mu1, Type mu2) {
  Type ans = log_dskellam(x, mu1, mu2);
  if (asDouble(x) >= 2.0)
    ans = logspace_add(ans,
		       LOG_ONE_THIRD
		       + log_dskellam(x - Type(2), mu1, mu2));
  if (asDouble(x) >= 1.0)
    ans = logspace_add(ans,
		       LOG_TWO_THIRDS
		       + log_dskellam(x - Type(1), mu1, mu2));
  ans = logspace_add(ans,
		     LOG_TWO_THIRDS
		     + log_dskellam(x + Type(1), mu1, mu2));
  ans = logspace_add(ans,
		     LOG_ONE_THIRD
		     + log_dskellam(x + Type(2), mu1, mu2));
  return ans;
}


template <class Type>
Type logpost_ar_inner(const vector<Type>& effectfree,
		      const vector<Type>& hyper,
		      const vector<Type>& consts,
		      const matrix<int>& matrix_along_by_effectfree) {
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
  vector<Type> coef_raw = invlogit(logit_coef);
  vector<Type> coef = (max - min) * coef_raw + min;
  Type sd = exp(log_sd);
  Type ans = Type(0);
  ans += dbeta(coef_raw, shape1, shape2, true).sum() +
    (log(coef_raw) + log1p(-coef_raw)).sum();
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
Type logpost_seasfix(const vector<Type>& seas,
		     const vector<Type>& consts) {
  Type sd_seas = consts[1];
  return dnorm(seas, Type(0), sd_seas, true).sum();
}

template <class Type>
Type logpost_seasvary(const vector<Type>& seas,
		      const vector<Type>& hyper,
		      const vector<Type>& consts,
		      const matrix<int>& matrix_along_by_effectfree) {
  int n_seas = CppAD::Integer(consts[0]);
  Type scale_innov = consts[1];
  Type sd_init = consts[2];
  Type log_sd_innov = hyper[0];
  Type sd_innov = exp(log_sd_innov);
  int n_by = matrix_along_by_effectfree.cols();
  int n_along_seas = seas.size() / n_by;
  Type ans = Type(0);
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
Type logpost_slope(const vector<Type>& slope,
		   const vector<Type>& consts) {
  Type mean_slope = consts[0];
  Type sd_slope = consts[1];
  return dnorm(slope, mean_slope, sd_slope, true).sum();
}


// "Methods" for 'logpost' functions for priors  ------------------------------

// Assume inputs all valid (checking done in R).
// Note that the 'Known' prior does not have
// an associated 'logpost' method.

template <class Type>
Type logpost_ar(const vector<Type>& effectfree,
		const vector<Type>& hyper,
		const vector<Type>& consts,
		const matrix<int>& matrix_along_by_effectfree) {
  return logpost_ar_inner(effectfree, hyper, consts, matrix_along_by_effectfree);
}

template <class Type>
Type logpost_lin(const vector<Type>& effectfree,
		 const vector<Type>& hyper,
		 const vector<Type>& hyperrandfree, // slope
		 const vector<Type>& consts,
		 const matrix<int>& matrix_along_by_effectfree) {
  Type scale = consts[0];
  Type mean_slope = consts[1];
  Type sd_slope = consts[2];
  int n_along = matrix_along_by_effectfree.rows();
  int n_by = matrix_along_by_effectfree.cols();
  Type log_sd = hyper[0];
  vector<Type> intercept = -0.5 * (n_along + 1.0) * hyperrandfree;
  Type sd = exp(log_sd);
  Type ans = Type(0);
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
Type logpost_linar(const vector<Type>& effectfree,
		   const vector<Type>& hyper,
		   const vector<Type>& hyperrandfree, // slope
		   const vector<Type>& consts,
		   const matrix<int>& matrix_along_by_effectfree) {
  vector<Type> consts_slope = consts.head(2);
  vector<Type> consts_ar = consts.tail(5);
  vector<Type> ar = alpha_lin(effectfree, hyperrandfree, matrix_along_by_effectfree);
  Type ans = Type(0);
  ans += logpost_slope(hyperrandfree, consts_slope);
  ans += logpost_ar_inner(ar, hyper, consts_ar, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_linex(const vector<Type>& effectfree, // slope
		   const vector<Type>& consts,
		   const matrix<int>& matrix_along_by_effectfree) {
  Type mean_slope = consts[0];
  Type sd_slope = consts[1];
  return dnorm(effectfree, mean_slope, sd_slope, true).sum();
}

template <class Type>
Type logpost_norm(const vector<Type>& effectfree,
		  const vector<Type>& hyper,
		  const vector<Type>& consts,
		  const matrix<int>& matrix_along_by_effectfree) {
  Type scale = consts[0];
  Type log_sd = hyper[0];
  Type sd = exp(log_sd);
  Type ans = Type(0);
  ans += dnorm(sd, Type(0), scale, true) + log_sd;
  ans += dnorm(effectfree, Type(0), sd, true).sum();
  return ans;
}

template <class Type>
Type logpost_normfixed(const vector<Type>& effectfree,
		       const vector<Type>& consts,
		       const matrix<int>& matrix_along_by_effectfree) {
  Type sd = consts[0];
  Type ans = Type(0);
  ans += dnorm(effectfree, Type(0), sd, true).sum();
  return ans;
}

template <class Type>
Type logpost_rwrandom(const vector<Type>& rw,
		      const vector<Type>& hyper,
		      const vector<Type>& consts,
		      const matrix<int>& matrix_along_by) {
  Type scale_innov = consts[0];
  Type sd_init = consts[1];
  Type log_sd_innov = hyper[0];
  Type sd_innov = exp(log_sd_innov);
  int n_along = matrix_along_by.rows();
  int n_by = matrix_along_by.cols();
  Type ans = Type(0);
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
Type logpost_rwrandomseasfix(const vector<Type>& effectfree,
			     const vector<Type>& hyper,
			     const vector<Type>& hyperrandfree, // seasonal effect
			     const vector<Type>& consts,
			     const matrix<int>& matrix_along_by_effectfree) {
  vector<Type> consts_seas = consts.head(2); // n_seas, sd_seas
  vector<Type> consts_rw(2);
  consts_rw[0] = consts[2]; // scale
  consts_rw[1] = consts[1]; // sd_seas 
  vector<Type> rw = alpha_randomseasfix(effectfree,
					hyperrandfree,
					consts_seas,
					matrix_along_by_effectfree);
  Type ans = Type(0);
  ans += logpost_seasfix(hyperrandfree, consts_seas);
  ans += logpost_rwrandom(rw, hyper, consts_rw, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_rwrandomseasvary(const vector<Type>& effectfree,
			      const vector<Type>& hyper,
			      const vector<Type>& hyperrandfree, // seasonal effect
			      const vector<Type>& consts,
			      const matrix<int>& matrix_along_by_effectfree) {
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
  Type ans = Type(0);
  ans += logpost_seasvary(hyperrandfree, hyper_seas, consts_seas, matrix_along_by_effectfree);
  ans += logpost_rwrandom(rw, hyper_rw, consts_rw, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_rwzero(const vector<Type>& rw,
		    const vector<Type>& hyper,
		    const vector<Type>& consts,
		    const matrix<int>& matrix_along_by) {
  Type scale_innov = consts[0];
  Type log_sd_innov = hyper[0];
  Type sd_innov = exp(log_sd_innov);
  int n_along = matrix_along_by.rows();
  int n_by = matrix_along_by.cols();
  Type ans = Type(0);
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
Type logpost_rwzeroseasfix(const vector<Type>& effectfree,
			   const vector<Type>& hyper,
			   const vector<Type>& hyperrandfree, // seasonal effect
			   const vector<Type>& consts,
			   const matrix<int>& matrix_along_by_effectfree) {
  vector<Type> consts_seas = consts.head(2); // n_seas, sd_seas
  vector<Type> consts_rw(2);
  consts_rw[0] = consts[2]; // scale
  consts_rw[1] = consts[1]; // sd_seas 
  vector<Type> rw = alpha_zeroseasfix(effectfree,
				      hyperrandfree,
				      consts_seas,
				      matrix_along_by_effectfree);
  Type ans = Type(0);
  ans += logpost_seasfix(hyperrandfree, consts_seas);
  ans += logpost_rwrandom(rw, hyper, consts_rw, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_rwzeroseasvary(const vector<Type>& effectfree,
			    const vector<Type>& hyper,
			    const vector<Type>& hyperrandfree, // seasonal effect
			    const vector<Type>& consts,
			    const matrix<int>& matrix_along_by_effectfree) {
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
  Type ans = Type(0);
  ans += logpost_seasvary(hyperrandfree, hyper_seas, consts_seas, matrix_along_by_effectfree);
  ans += logpost_rwrandom(rw, hyper_rw, consts_rw, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_rw2infant(const vector<Type>& effectfree,
		       const vector<Type>& hyper,
		       const vector<Type>& consts,
		       const matrix<int>& matrix_along_by_effectfree) {
  Type scale_innov = consts[0];
  Type sd_slope = consts[1];
  Type log_sd_innov = hyper[0];
  Type sd_innov = exp(log_sd_innov);
  int n_along = matrix_along_by_effectfree.rows();
  int n_by = matrix_along_by_effectfree.cols();
  Type ans = Type(0);
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
Type logpost_rw2random(const vector<Type>& rw,
		       const vector<Type>& hyper,
		       const vector<Type>& consts,
		       const matrix<int>& matrix_along_by) {
  Type scale_innov = consts[0];
  Type sd_init = consts[1];
  Type sd_slope = consts[2];
  Type log_sd_innov = hyper[0];
  Type sd_innov = exp(log_sd_innov);
  int n_along = matrix_along_by.rows();
  int n_by = matrix_along_by.cols();
  Type ans = Type(0);
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
Type logpost_rw2randomseasfix(const vector<Type>& effectfree,
			      const vector<Type>& hyper,
			      const vector<Type>& hyperrandfree, // seasonal effect
			      const vector<Type>& consts,
			      const matrix<int>& matrix_along_by_effectfree) {
  vector<Type> consts_seas = consts.head(2); // n_seas, sd_seas
  vector<Type> consts_rw = consts.tail(3); // scale, sd, sd_slope
  vector<Type> rw = alpha_randomseasfix(effectfree,
					hyperrandfree,
					consts_seas,
					matrix_along_by_effectfree);
  Type ans = Type(0);
  ans += logpost_seasfix(hyperrandfree, consts_seas);
  ans += logpost_rw2random(rw, hyper, consts_rw, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_rw2randomseasvary(const vector<Type>& effectfree,
			       const vector<Type>& hyper,
			       const vector<Type>& hyperrandfree, // seasonal effect
			       const vector<Type>& consts,
			       const matrix<int>& matrix_along_by_effectfree) {
  vector<Type> consts_seas = consts.head(3); // n_seas, scale_seas, sd_seas
  vector<Type> consts_rw = consts.tail(3); // scale, sd, sd_slope
  vector<Type> hyper_seas = hyper.head(1); // log_sd_seas
  vector<Type> hyper_rw = hyper.tail(1); // log_sd
  vector<Type> rw = alpha_randomseasvary(effectfree,
					 hyperrandfree,
					 consts_seas,
					 matrix_along_by_effectfree);
  Type ans = Type(0);
  ans += logpost_seasvary(hyperrandfree, hyper_seas, consts_seas, matrix_along_by_effectfree);
  ans += logpost_rw2random(rw, hyper_rw, consts_rw, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_rw2zero(const vector<Type>& rw,
		     const vector<Type>& hyper,
		     const vector<Type>& consts,
		     const matrix<int>& matrix_along_by) {
  Type scale_innov = consts[0];
  Type sd_slope = consts[1];
  Type log_sd_innov = hyper[0];
  Type sd_innov = exp(log_sd_innov);
  int n_along = matrix_along_by.rows();
  int n_by = matrix_along_by.cols();
  Type ans = Type(0);
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
Type logpost_rw2zeroseasfix(const vector<Type>& effectfree,
			    const vector<Type>& hyper,
			    const vector<Type>& hyperrandfree, // seasonal effect
			    const vector<Type>& consts,
			    const matrix<int>& matrix_along_by_effectfree) {
  vector<Type> consts_seas = consts.head(2); // n_seas, sd_seas
  vector<Type> consts_rw(3);
  consts_rw[0] = consts[2]; // scale
  consts_rw[1] = consts[1]; // sd_seas, used for sd
  consts_rw[2] = consts[3]; // sd_slope
  vector<Type> rw = alpha_zeroseasfix(effectfree,
				      hyperrandfree,
				      consts_seas,
				      matrix_along_by_effectfree);
  Type ans = Type(0);
  ans += logpost_seasfix(hyperrandfree, consts_seas);
  ans += logpost_rw2random(rw, hyper, consts_rw, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_rw2zeroseasvary(const vector<Type>& effectfree,
			     const vector<Type>& hyper,
			     const vector<Type>& hyperrandfree, // seasonal effect
			     const vector<Type>& consts,
			     const matrix<int>& matrix_along_by_effectfree) {
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
  Type ans = Type(0);
  ans += logpost_seasvary(hyperrandfree, hyper_seas, consts_seas, matrix_along_by_effectfree);
  ans += logpost_rw2random(rw, hyper_rw, consts_rw, matrix_along_by_effectfree);
  return ans;
}

template <class Type>
Type logpost_spline(const vector<Type>& effectfree,
   		    const vector<Type>& hyper,
		    const vector<Type>& consts,
		    const matrix<int>& matrix_along_by_effectfree) {
  return logpost_rw2random(effectfree, hyper, consts, matrix_along_by_effectfree);
}

template <class Type>
Type logpost_svd(const vector<Type>& effectfree,
		 const vector<Type>& consts,
		 const matrix<int>& matrix_along_by_effectfree) {
  return dnorm(effectfree, Type(0), Type(1), true).sum();
}

template <class Type>
Type logpost_svd_ar(const vector<Type>& effectfree,
		    const vector<Type>& hyper,
		    const vector<Type>& consts,
		    const matrix<int>& matrix_along_by_effectfree) {
  return logpost_ar_inner(effectfree, hyper, consts, matrix_along_by_effectfree);
}

template <class Type>
Type logpost_svd_rwrandom(const vector<Type>& effectfree,
			  const vector<Type>& hyper,
			  const vector<Type>& consts,
			  const matrix<int>& matrix_along_by_effectfree) {
  return logpost_rwrandom(effectfree, hyper, consts, matrix_along_by_effectfree);
}

template <class Type>
Type logpost_svd_rwzero(const vector<Type>& effectfree,
			const vector<Type>& hyper,
			const vector<Type>& consts,
			const matrix<int>& matrix_along_by_effectfree) {
  return logpost_rwzero(effectfree, hyper, consts, matrix_along_by_effectfree);
}

template <class Type>
Type logpost_svd_rw2random(const vector<Type>& effectfree,
			   const vector<Type>& hyper,
			   const vector<Type>& consts,
			   const matrix<int>& matrix_along_by_effectfree) {
  return logpost_rw2random(effectfree, hyper, consts, matrix_along_by_effectfree);
}

template <class Type>
Type logpost_svd_rw2zero(const vector<Type>& effectfree,
			 const vector<Type>& hyper,
			 const vector<Type>& consts,
			 const matrix<int>& matrix_along_by_effectfree) {
  return logpost_rw2zero(effectfree, hyper, consts, matrix_along_by_effectfree);
}


// Equivalent of method dispatch for logpost functions for priors -------------

template <class Type>
Type logpost_no_hyper(const vector<Type>& effectfree,
			    const vector<Type>& consts,
			    const matrix<int>& matrix_along_by_effectfree,
			    int i_prior) {
  Type ans = Type(0);
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
    Rf_error("Internal error: function 'logpost_no_hyper' cannot handle i_prior = %d", i_prior); // # nocov
  }
  return ans;
}

template <class Type>
Type logpost_has_hyper(const vector<Type>& effectfree,
			const vector<Type>& hyper,
			const vector<Type>& consts,
			const matrix<int>& matrix_along_by_effectfree,
			int i_prior) {
  Type ans = Type(0);
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
    Rf_error("Internal error: function 'logpost_has_hyper' cannot handle i_prior = %d", i_prior); // # nocov
  }
  return ans;
}

template <class Type>
Type logpost_has_hyperrandfree(const vector<Type>& effectfree,
				const vector<Type>& hyper,
				const vector<Type>& hyperrandfree,
				const vector<Type>& consts,
				const matrix<int>& matrix_along_by_effectfree,
				int i_prior) {
  Type ans = Type(0);
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
    Rf_error("Internal error: function 'logpost_has_hyperrandfree' cannot handle i_prior = %d", i_prior); // # nocov
  }
  return ans;
}


// 'Methods' for logpost function for data models -----------------------------

template <class Type>
Type logpost_datamod_miscount(const vector<Type>& datamod_param) {
  const vector<Type>& z = datamod_param;
  return dnorm(z, Type(0), Type(1), true).sum();
}

template <class Type>
Type logpost_datamod_overcount(const vector<Type>& datamod_param) {
  const vector<Type>& z_rate = datamod_param;
  return dnorm(z_rate, Type(0), Type(1), true).sum();
}


template <class Type>
Type logpost_datamod_undercount(const vector<Type>& datamod_param) {
  const vector<Type>& z_prob = datamod_param;
  return dnorm(z_prob, Type(0), Type(1), true).sum();
}


// Equivalent of method dispatch for logpost function for data models ---------

template <class Type>
Type logpost_datamod(const vector<Type>& datamod_param,
		     int i_datamod) {
  Type ans = Type(0);
  switch(i_datamod) {
  case 1000:
    ans = 0; // no parameters
    break;
  case 2000:
    ans = logpost_datamod_miscount(datamod_param);
    break;
  case 3000:
    ans = 0; // no parameters
    break;
  case 4000:
    ans = logpost_datamod_overcount(datamod_param);
    break;
  case 5000:
    ans = logpost_datamod_undercount(datamod_param);
    break;
  default: // # nocov
    Rf_error("Internal error: 'logpost_datamod' cannot handle i_datmod = %d", // # nocov
	  i_datamod); // # nocov
  }
  return ans;
}


// 'Methods' for fill_datamod_vals function for data models -------------------

template <class Type>
void fill_datamod_vals_exposure(MatrixD<Type> &datamod_vals,
				const vector<Type>& datamod_consts,
				const LIST_SM_t<Type> &datamod_matrices) {
  const vector<Type>& disp = datamod_consts;
  const SparseMatrix<Type>& disp_matrix = datamod_matrices(0);
  const int n_outcome = disp_matrix.rows();
  datamod_vals.resize(n_outcome, 1);
  datamod_vals.col(0) = disp_matrix * disp;
}

template <class Type>
void fill_datamod_vals_miscount(MatrixD<Type> &datamod_vals,
				const vector<Type>& datamod_param,
				 const vector<Type>& datamod_consts,
				const LIST_SM_t<Type> &datamod_matrices) {
  const SparseMatrix<Type>& prob_matrix = datamod_matrices(0);
  const SparseMatrix<Type>& rate_matrix = datamod_matrices(1);
  const int n_prob = prob_matrix.cols();
  const int n_rate = rate_matrix.cols();
  const int n_outcome = prob_matrix.rows();
  const vector<Type> z_prob = datamod_param.head(n_prob);
  const vector<Type> z_rate = datamod_param.tail(n_rate);
  const auto& prob_mean = datamod_consts.head(n_prob);
  const auto& prob_disp = datamod_consts.segment(n_prob, n_prob);
  const auto& rate_mean = datamod_consts.segment(2 * n_prob, n_rate);
  const auto& rate_disp = datamod_consts.tail(n_rate);
  const vector<Type> shape1 = prob_mean / prob_disp;
  const vector<Type> shape2 = (Type(1) - prob_mean) / prob_disp;
  const vector<Type> shape = rate_disp.cwiseInverse();
  const vector<Type> scale = rate_mean.cwiseProduct(rate_disp);
  vector<Type> u_prob = pnorm(z_prob, Type(0), Type(1));
  vector<Type> u_rate = pnorm(z_rate, Type(0), Type(1));
  const Type eps = Type(1e-12);
  u_prob = u_prob.cwiseMax(eps).cwiseMin(Type(1) - eps);
  u_rate = u_rate.cwiseMax(eps).cwiseMin(Type(1) - eps);
  const vector<Type> prob = qbeta(u_prob, shape1, shape2);
  const vector<Type> rate = qgamma(u_rate, shape, scale);
  datamod_vals.resize(n_outcome, 2);
  datamod_vals.col(0) = prob_matrix * prob;
  datamod_vals.col(1) = rate_matrix * rate;
}

template <class Type>
void fill_datamod_vals_noise(MatrixD<Type> &datamod_vals,
			     const vector<Type>& datamod_consts,
			     const LIST_SM_t<Type> &datamod_matrices) {
  const vector<Type>& sd = datamod_consts;
  const SparseMatrix<Type>& sd_matrix = datamod_matrices(0);
  const int n_outcome = sd_matrix.rows();
  datamod_vals.resize(n_outcome, 1);
  datamod_vals.col(0) = sd_matrix * sd;
}

template <class Type>
void fill_datamod_vals_overcount(MatrixD<Type> &datamod_vals,
				 const vector<Type>& datamod_param,
				 const vector<Type>& datamod_consts,
				 const LIST_SM_t<Type> &datamod_matrices) {
  const vector<Type>& z_rate = datamod_param;
  const int n_param = datamod_param.size();
  const vector<Type> rate_mean = datamod_consts.head(n_param);
  const vector<Type> rate_disp = datamod_consts.tail(n_param);
  const SparseMatrix<Type>& rate_matrix = datamod_matrices(0);
  int n_outcome = rate_matrix.rows();
  const vector<Type> shape = rate_disp.cwiseInverse();
  const vector<Type> scale = rate_mean * rate_disp;
  vector<Type> u_rate = pnorm(z_rate, Type(0), Type(1));
  const Type eps = Type(1e-12);
  u_rate = u_rate.cwiseMax(eps).cwiseMin(Type(1) - eps);
  const vector<Type> rate = qgamma(u_rate, shape, scale);
  datamod_vals.resize(n_outcome, 1);
  datamod_vals.col(0) = rate_matrix * rate;
}

template <class Type>
void fill_datamod_vals_undercount(MatrixD<Type> &datamod_vals,
				  const vector<Type>& datamod_param,
				  const vector<Type>& datamod_consts,
				  const LIST_SM_t<Type> &datamod_matrices) {
  const vector<Type>& z_prob = datamod_param;
  const int n_param = datamod_param.size();
  const vector<Type> prob_mean = datamod_consts.head(n_param);
  const vector<Type> prob_disp = datamod_consts.tail(n_param);
  const SparseMatrix<Type>& prob_matrix = datamod_matrices(0);
  int n_outcome = prob_matrix.rows();
  const vector<Type> shape1 = prob_mean / prob_disp;
  const vector<Type> shape2 = (Type(1) - prob_mean) / prob_disp;
  vector<Type> u_prob = pnorm(z_prob, Type(0), Type(1));
  const Type eps = Type(1e-12);
  u_prob = u_prob.cwiseMax(eps).cwiseMin(Type(1) - eps);
  const vector<Type> prob = qbeta(u_prob, shape1, shape2);
  datamod_vals.resize(n_outcome, 1);
  datamod_vals.col(0) = prob_matrix * prob;
}

// Equivalent of method dispatch for fill_datamod_vals ------------------------

template <class Type>
void fill_datamod_vals(MatrixD<Type> &datamod_vals,
		       const vector<Type>& datamod_param,
		       const vector<Type>& datamod_consts,
		       const LIST_SM_t<Type> &datamod_matrices,
		       int i_datamod) {
  switch(i_datamod) {
  case 1000:
    fill_datamod_vals_exposure(datamod_vals,
			       datamod_consts,
			       datamod_matrices);
    break;
  case 2000:
    fill_datamod_vals_miscount(datamod_vals,
			       datamod_param,
			       datamod_consts,
			       datamod_matrices);
    break;
  case 3000:
    fill_datamod_vals_noise(datamod_vals,
			    datamod_consts,
			    datamod_matrices);
    break;
  case 4000:
    fill_datamod_vals_overcount(datamod_vals,
				datamod_param,
				datamod_consts,
				datamod_matrices);
    break;
  case 5000:
    fill_datamod_vals_undercount(datamod_vals,
				 datamod_param,
				 datamod_consts,
				 datamod_matrices);
    break;
  default: // # nocov
    Rf_error("Internal error: 'fill_datamod_vals' cannot handle i_datamod = %d", // # nocov
	  i_datamod); // # nocov
  }
}



// 'Methods' for loglik -------------------------------------------------------


// No disp, no data models

template <class Type>
Type loglik_pois_no_disp(Type outcome,
			 Type linpred,
			 Type offset) {
  Type rate = exp(linpred) * offset;
  return dpois(outcome, rate, true);
}

template <class Type>
Type loglik_pois_no_disp_rr3(Type outcome,
			     Type linpred,
			     Type offset) {
  Type rate = exp(linpred) * offset;
  return log_dpois_rr3(outcome, rate);
}

template <class Type>
Type loglik_binom_no_disp(Type outcome,
			  Type linpred,
			  Type offset) {
  return dbinom_robust(outcome, offset, linpred, true);
}

template <class Type>
Type loglik_binom_no_disp_rr3(Type outcome,
			      Type linpred,
			      Type offset) {
  return log_dbinom_robust_rr3(outcome, offset, linpred);
}


// Has disp, no data models

template <class Type>
Type loglik_pois_has_disp(Type outcome,
			  Type linpred,
			  Type offset,
			  Type disp) {
  Type mu = exp(linpred) * offset;
  Type size = Type(1) / disp;
  return dnbinom_mu(outcome, size, mu, true);
}

template <class Type>
Type loglik_pois_has_disp_rr3(Type outcome,
			      Type linpred,
			      Type offset,
			      Type disp) {
  Type mu = exp(linpred) * offset;
  Type size = Type(1) / disp;
  return log_dnbinom_mu_rr3(outcome, size, mu);
}

template <class Type>
Type loglik_binom_has_disp(Type outcome,
			   Type linpred,
			   Type offset,
			   Type disp) {
  Type mu = invlogit(linpred);
  return log_dbetabinom(outcome, offset, mu, disp);
}

template <class Type>
Type loglik_binom_has_disp_rr3(Type outcome,
			       Type linpred,
			       Type offset,
			       Type disp) {
  Type mu = invlogit(linpred);
  return log_dbetabinom_rr3(outcome, offset, mu, disp);
}

template <class Type>
Type loglik_norm(Type outcome,
		 Type linpred,
		 Type offset,
		 Type disp) {
  Type sd = disp / sqrt(offset);            // on transformed scale
  return dnorm(outcome, linpred, sd, true); // on transformed scale
}


// No disp, has data models

template <class Type>
Type loglik_pois_no_disp_exposure(Type outcome,
				  const vector<Type>& datamod_vals,
				  Type linpred,
				  Type offset) {
  Type d_inv = 1 / datamod_vals[0];
  Type size = Type(3) + d_inv;
  Type prob = (1 + d_inv) / (1 + d_inv + exp(linpred) * offset);
  return dnbinom(outcome, size, prob, true);
}


template <class Type>
Type loglik_pois_no_disp_exposure_rr3(Type outcome,
				      const vector<Type>& datamod_vals,
				      Type linpred,
				      Type offset) {
  Type d_inv  = 1 / datamod_vals[0];
  Type size = Type(3) + d_inv;
  Type prob = (1 + d_inv) / (1 + d_inv + exp(linpred) * offset);
  return log_dnbinom_rr3(outcome, size, prob);
}

template <class Type>
Type loglik_pois_no_disp_miscount(Type outcome,
				  const vector<Type>& datamod_vals,
				  Type linpred,
				  Type offset) {
  Type p = datamod_vals[0];
  Type r = datamod_vals[1];
  Type lambda = (p + r) * exp(linpred) * offset;
  return dpois(outcome, lambda, true);
}

template <class Type>
Type loglik_pois_no_disp_miscount_rr3(Type outcome,
				      const vector<Type>& datamod_vals,
				      Type linpred,
				      Type offset) {
  Type p = datamod_vals[0];
  Type r = datamod_vals[1];
  Type lambda = (p + r) * exp(linpred) * offset;
  return log_dpois_rr3(outcome, lambda);
}

template <class Type>
Type loglik_pois_no_disp_noise(Type outcome,
			       const vector<Type>& datamod_vals,
			       Type linpred,
			       Type offset) {
  Type s = datamod_vals[0];
  Type mu1 = exp(linpred) * offset + Type(0.5) * s * s;
  Type mu2 = Type(0.5) * s * s;
  return log_dskellam(outcome, mu1, mu2);
}

template <class Type>
Type loglik_pois_no_disp_noise_rr3(Type outcome,
				   const vector<Type>& datamod_vals,
				   Type linpred,
				   Type offset) {
  Type s = datamod_vals[0];
  Type mu1 = exp(linpred) * offset + Type(0.5) * s * s;
  Type mu2 = Type(0.5) * s * s;
  return log_dskellam_rr3(outcome, mu1, mu2);
}

template <class Type>
Type loglik_pois_no_disp_overcount(Type outcome,
				  const vector<Type>& datamod_vals,
				  Type linpred,
				  Type offset) {
  Type r = datamod_vals[0];
  Type lambda = (Type(1) + r) * exp(linpred) * offset;
  return dpois(outcome, lambda, true);
}

template <class Type>
Type loglik_pois_no_disp_overcount_rr3(Type outcome,
				       const vector<Type>& datamod_vals,
				       Type linpred,
				       Type offset) {
  Type r = datamod_vals[0];
  Type lambda = (Type(1) + r) * exp(linpred) * offset;
  return log_dpois_rr3(outcome, lambda);
}

template <class Type>
Type loglik_pois_no_disp_undercount(Type outcome,
				    const vector<Type>& datamod_vals,
				    Type linpred,
				    Type offset) {
  Type p = datamod_vals[0];
  Type lambda = p * exp(linpred) * offset;
  return dpois(outcome, lambda, true);
}

template <class Type>
Type loglik_pois_no_disp_undercount_rr3(Type outcome,
					const vector<Type>& datamod_vals,
					Type linpred,
					Type offset) {
  Type p = datamod_vals[0];
  Type lambda = p * exp(linpred) * offset;
  return log_dpois_rr3(outcome, lambda);
}

template <class Type>
Type loglik_binom_no_disp_undercount(Type outcome,
				    const vector<Type>& datamod_vals,
				    Type linpred,
				    Type offset) {
  Type p = datamod_vals[0];
  Type prob = p * invlogit(linpred);
  return dbinom(outcome, offset, prob, true);
}

template <class Type>
Type loglik_binom_no_disp_undercount_rr3(Type outcome,
					 const vector<Type>& datamod_vals,
					 Type linpred,
					 Type offset) {
  Type p = datamod_vals[0];
  Type prob = p * invlogit(linpred);
  return log_dbinom_rr3(outcome, offset, prob);
}


// Has disp, has data models

template <class Type>
Type loglik_pois_has_disp_miscount(Type outcome,
				   const vector<Type>& datamod_vals,
				   Type linpred,
				   Type offset,
				   Type disp) {
  Type p = datamod_vals[0];
  Type r = datamod_vals[1];
  Type mu = (p + r) * exp(linpred) * offset;
  Type size = Type(1) / disp;
  return dnbinom_mu(outcome, size, mu, true);
}

template <class Type>
Type loglik_pois_has_disp_miscount_rr3(Type outcome,
				       const vector<Type>& datamod_vals,
				       Type linpred,
				       Type offset,
				       Type disp) {
  Type p = datamod_vals[0];
  Type r = datamod_vals[1];
  Type mu = (p + r) * exp(linpred) * offset;
  Type size = Type(1) / disp;
  return log_dnbinom_mu_rr3(outcome, size, mu);
}

template <class Type>
Type loglik_pois_has_disp_overcount(Type outcome,
				    const vector<Type>& datamod_vals,
				    Type linpred,
				    Type offset,
				    Type disp) {
  Type r = datamod_vals[0];
  Type mu = (Type(1) + r) * exp(linpred) * offset;
  Type size = Type(1) / disp;
  return dnbinom_mu(outcome, size, mu, true);
}

template <class Type>
Type loglik_pois_has_disp_overcount_rr3(Type outcome,
					const vector<Type>& datamod_vals,
					Type linpred,
					Type offset,
					Type disp) {
  Type r = datamod_vals[0];
  Type mu = (Type(1) + r) * exp(linpred) * offset;
  Type size = Type(1) / disp;
  return log_dnbinom_mu_rr3(outcome, size, mu);
}

template <class Type>
Type loglik_pois_has_disp_undercount(Type outcome,
				     const vector<Type>& datamod_vals,
				     Type linpred,
				     Type offset,
				     Type disp) {
  Type p = datamod_vals[0];
  Type mu = p * exp(linpred) * offset;
  Type size = Type(1) / disp;
  return dnbinom_mu(outcome, size, mu, true);
}

template <class Type>
Type loglik_pois_has_disp_undercount_rr3(Type outcome,
					 const vector<Type>& datamod_vals,
					 Type linpred,
					 Type offset,
					 Type disp) {
  Type p = datamod_vals[0];
  Type mu = p * exp(linpred) * offset;
  Type size = Type(1) / disp;
  return log_dnbinom_mu_rr3(outcome, size, mu);
}

template <class Type>
Type loglik_binom_has_disp_undercount(Type outcome,
				      const vector<Type>& datamod_vals,
				      Type linpred,
				      Type offset,
				      Type disp) {
  Type p = datamod_vals[0];
  Type mu = p * invlogit(linpred);
  return log_dbetabinom(outcome, offset, mu, disp);
}

template <class Type>
Type loglik_binom_has_disp_undercount_rr3(Type outcome,
					  const vector<Type>& datamod_vals,
					  Type linpred,
					  Type offset,
					  Type disp) {
  Type p = datamod_vals[0];
  Type mu = p * invlogit(linpred);
  return log_dbetabinom_rr3(outcome, offset, mu, disp);
}

template <class Type>
Type loglik_norm_noise(Type outcome,
		       const vector<Type>& datamod_vals,
		       Type linpred,
		       Type offset,
		       Type disp) {
  const Type s = datamod_vals[0];                // on transformed scale
  const Type var = disp * disp / offset + s * s; // on transformed scale
  const Type sd = sqrt(var);                     // on transformed scale
  return dnorm(outcome, linpred, sd, true);
}


// Equivalent of method dispatch for loglik -----------------------------------

template <class Type>
Type loglik_no_disp_no_dm(Type outcome,
			  Type linpred,
			  Type offset,
			  int i_lik) {
  Type ans = Type(0);
  switch(i_lik) {
  case 200000:
    ans = loglik_pois_no_disp(outcome, linpred, offset); 
    break;
  case 200010:
    ans = loglik_pois_no_disp_rr3(outcome, linpred, offset); 
    break;
  case 400000:
    ans = loglik_binom_no_disp(outcome, linpred, offset); 
    break;
  case 400010:
    ans = loglik_binom_no_disp_rr3(outcome, linpred, offset); 
    break;
  default: // # nocov
    Rf_error("Internal error: 'loglik_no_disp_no_dm' cannot handle i_lik = %d",  // # nocov
	  i_lik); // # nocov
  }
  return ans;
}

template <class Type>
Type loglik_has_disp_no_dm(Type outcome,
			   Type linpred,
			   Type offset,
			   Type disp,
			   int i_lik) {
  Type ans = Type(0);
  switch(i_lik) {
  case 100000:
    ans = loglik_pois_has_disp(outcome, linpred, offset, disp); 
    break;
  case 100010:
    ans = loglik_pois_has_disp_rr3(outcome, linpred, offset, disp); 
    break;
  case 300000:
    ans = loglik_binom_has_disp(outcome, linpred, offset, disp); 
    break;
  case 300010:
    ans = loglik_binom_has_disp_rr3(outcome, linpred, offset, disp); 
    break;
  case 500000:
    ans = loglik_norm(outcome, linpred, offset, disp); 
    break;
  default: // # nocov
    Rf_error("Internal error: function 'loglik_has_disp_no_dm' cannot handle i_lik = %d",  // # nocov
	  i_lik); // # nocov
  }
  return ans;
}

template <class Type>
Type loglik_no_disp_has_dm(Type outcome,
			   const vector<Type>& datamod_vals,
			   Type linpred,
			   Type offset,
			   int i_lik) {
  Type ans = Type(0);
  switch(i_lik) {
  case 201000:
    ans = loglik_pois_no_disp_exposure(outcome,
				       datamod_vals,
				       linpred,
				       offset);
    break;
  case 201010:
    ans = loglik_pois_no_disp_exposure_rr3(outcome,
					   datamod_vals,
					   linpred,
					   offset);
    break;
  case 202000:
    ans = loglik_pois_no_disp_miscount(outcome,
				       datamod_vals,
				       linpred,
				       offset);
    break;
  case 202010:
    ans = loglik_pois_no_disp_miscount_rr3(outcome,
					   datamod_vals,
					   linpred,
					   offset);
    break;
  case 203000:
    ans = loglik_pois_no_disp_noise(outcome,
				    datamod_vals,
				    linpred,
				    offset);
    break;
  case 203010:
    ans = loglik_pois_no_disp_noise_rr3(outcome,
					datamod_vals,
					linpred,
					offset);
    break;
  case 204000:
    ans = loglik_pois_no_disp_overcount(outcome,
					datamod_vals,
					linpred,
					offset);
    break;
  case 204010:
    ans = loglik_pois_no_disp_overcount_rr3(outcome,
					    datamod_vals,
					    linpred,
					    offset);
    break;
  case 205000:
    ans = loglik_pois_no_disp_undercount(outcome,
					 datamod_vals,
					 linpred,
					 offset);
    break;
  case 205010:
    ans = loglik_pois_no_disp_undercount_rr3(outcome,
					     datamod_vals,
					     linpred,
					     offset);
    break;
  case 405000:
    ans = loglik_binom_no_disp_undercount(outcome,
					  datamod_vals,
					  linpred,
					  offset);
    break;
  case 405010:
    ans = loglik_binom_no_disp_undercount_rr3(outcome,
					      datamod_vals,
					      linpred,
					      offset);
    break;
  default: // # nocov
    Rf_error("Internal error: 'loglik_has_disp_no_dm' cannot handle i_lik = %d",  // # nocov
	  i_lik); // # nocov
  }
  return ans;
}


template <class Type>
Type loglik_has_disp_has_dm(Type outcome,
			    const vector<Type>& datamod_vals,
			    Type linpred,
			    Type offset,
			    Type disp,
			    int i_lik) {
  Type ans = Type(0);
  switch(i_lik) {
  case 102000:
    ans = loglik_pois_has_disp_miscount(outcome,
					datamod_vals,
					linpred,
					offset,
					disp);
    break;
  case 102010:
    ans = loglik_pois_has_disp_miscount_rr3(outcome,
					    datamod_vals,
					    linpred,
					    offset,
					    disp);
    break;
  case 104000:
    ans = loglik_pois_has_disp_overcount(outcome,
					 datamod_vals,
					 linpred,
					 offset,
					 disp);
    break;
  case 104010:
    ans = loglik_pois_has_disp_overcount_rr3(outcome,
					     datamod_vals,
					     linpred,
					     offset,
					     disp);
    break;
  case 105000:
    ans = loglik_pois_has_disp_undercount(outcome,
					  datamod_vals,
					  linpred,
					  offset,
					  disp);
    break;
  case 105010:
    ans = loglik_pois_has_disp_undercount_rr3(outcome,
					      datamod_vals,
					      linpred,
					      offset,
					      disp);
    break;
  case 305000:
    ans = loglik_binom_has_disp_undercount(outcome,
					   datamod_vals,
					   linpred,
					   offset,
					   disp);
    break;
  case 305010:
    ans = loglik_binom_has_disp_undercount_rr3(outcome,
					       datamod_vals,
					       linpred,
					       offset,
					       disp);    
    break;
  case 503000:
    ans = loglik_norm_noise(outcome,
			    datamod_vals,
			    linpred,
			    offset,
			    disp);
    break;
  default: // # nocov
    Rf_error("Internal error: 'loglik_has_disp_hs_dm' cannot handle i_lik = %d",  // # nocov
	  i_lik); // # nocov
  }
  return ans;
}


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
  DATA_SPARSE_MATRIX(matrix_covariates);
  DATA_INTEGER(i_datamod);
  DATA_VECTOR(datamod_consts);
  DATA_STRUCT(datamod_matrices, LIST_SM_t);

  PARAMETER_VECTOR(effectfree); 
  PARAMETER_VECTOR(hyper);
  PARAMETER_VECTOR(hyperrandfree);
  PARAMETER(log_disp);
  PARAMETER_VECTOR(coef_covariates);
  PARAMETER_VECTOR(datamod_param);
  

  // intermediate quantities

  const int n_outcome = outcome.size();
  const int n_term = i_prior.size();
  vector<vector<Type> > effectfree_split = split(effectfree, terms_effectfree);
  vector<vector<Type> > hyper_split = split(hyper, terms_hyper);
  vector<vector<Type> > hyperrandfree_split = split(hyperrandfree, terms_hyperrandfree);
  vector<vector<Type> > consts_split = split(consts, terms_consts);
  const bool has_disp = mean_disp > 0;
  Type disp = has_disp ? exp(log_disp) : 0;
  const bool uses_covariates = matrix_covariates.cols() > 0;
  const bool has_datamod = i_datamod > 0;
  MatrixD<Type> datamod_vals;

  // linear predictor

  vector<Type> linpred(n_outcome);
  linpred.fill(0);
  for (int i_term = 0; i_term < n_term; i_term++) {
    const SparseMatrix<Type>& matrix_effect_outcome = matrices_effect_outcome[i_term];
    vector<Type> effectfree_term = effectfree_split[i_term];
    const int n_effect = matrix_effect_outcome.cols();
    vector<Type> effect_term(n_effect);
    if (uses_matrix_effectfree_effect[i_term]) {
      const SparseMatrix<Type>& matrix_effectfree_effect = matrices_effectfree_effect[i_term];
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
  if (uses_covariates) {
    linpred = linpred + matrix_covariates * coef_covariates;
  }


  if (has_datamod) {
    fill_datamod_vals(datamod_vals,
		      datamod_param,
		      datamod_consts,
		      datamod_matrices,
		      i_datamod);
  }
  
  // negative log posterior
  
  Type ans = Type(0);

  // contribution to log posterior from priors
  for (int i_term = 0; i_term < n_term; i_term++) {
    int i_prior_term = i_prior[i_term];
    if (i_prior_term > 0) { // i_prior_term == 0 when prior is "Known"
      vector<Type> effectfree_term = effectfree_split[i_term];
      const vector<Type> consts_term = consts_split[i_term];
      const matrix<int>& matrix_along_by_effectfree = matrices_along_by_effectfree[i_term];
      if (uses_hyper[i_term]) {
	vector<Type> hyper_term = hyper_split[i_term];
	if (uses_hyperrandfree[i_term]) { // if a prior uses hyperrandfree, then it uses hyper
	  vector<Type> hyperrandfree_term = hyperrandfree_split[i_term];
	  ans -= logpost_has_hyperrandfree(effectfree_term,
					   hyper_term,
					   hyperrandfree_term,
					   consts_term,
					   matrix_along_by_effectfree,
					   i_prior_term);
	}
	else {
	  ans -= logpost_has_hyper(effectfree_term,
				   hyper_term,
				   consts_term,
				   matrix_along_by_effectfree,
				   i_prior_term);
	}
      } 
      else { // not uses hyper
	ans -= logpost_no_hyper(effectfree_term,
				consts_term,
				matrix_along_by_effectfree,
				i_prior_term);
      }
    }
  }

  // contribution to log posterior from covariates
  if (uses_covariates) {
    ans -= dnorm(coef_covariates, Type(0), Type(1), true).sum();
  }
  
  // contribution to log posterior from dispersion term
  if (has_disp) {
    Type rate_disp = Type(1) / mean_disp;
    ans -= dexp(disp, rate_disp, true);
    ans -= log_disp; // Jacobian
  }

  // contribution to log posterior from data model
  if (has_datamod) {
    ans -= logpost_datamod(datamod_param,
			   i_datamod);
  }
  
  // contribution to log posterior from data
  for (int i_outcome = 0; i_outcome < n_outcome; i_outcome++) {
    Type out = outcome[i_outcome];
    Type lin = linpred[i_outcome];
    Type off = offset[i_outcome];
    if (has_datamod) {
      const vector<Type>& dv = datamod_vals.row(i_outcome);
      if (has_disp)
	ans -= loglik_has_disp_has_dm(out, dv, lin, off, disp, i_lik);
      else
	ans -= loglik_no_disp_has_dm(out, dv, lin, off, i_lik);
    }
    else {
      if (has_disp)
	ans -= loglik_has_disp_no_dm(out, lin, off, disp, i_lik);
      else
	ans -= loglik_no_disp_no_dm(out, lin, off, i_lik);
    }
  }
  return ans;
}
