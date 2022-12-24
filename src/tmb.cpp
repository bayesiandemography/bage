
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_ARRAY(x);

  PARAMETER(mu);

  int n = x.size();

  Type ans = -1 * dnorm(x, mu, 1, true).sum();
  
  return ans;
}
