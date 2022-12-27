#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS
#include <TMB.hpp>


template <class Type>
Type logpost_norm(vector<Type> x, Type mu) {
  return dnorm(x, mu, 1, true).sum();
}


template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(x);

  PARAMETER(mu);

  Type ans = -1 * logpost_norm(x, mu);

  return ans;
}






