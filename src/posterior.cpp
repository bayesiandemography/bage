#include <cpp11.hpp>
#include <cpp11/matrix.hpp>
#include <cpp11/doubles.hpp>
using namespace cpp11;
namespace writable = cpp11::writable;

// HAS_TESTS
//' Assemble effects involving age for a single
//' draw from the posterior distribution
//'
//' Assemble terms that represent an age or age-sex
//' effect within a subspace, then transform back
//' to the original space. The calculations are
//' carried out on a single draw from the posterior
//' distribution.
//'
//' Each element of \code{terms} is composed
//' of one or more vectors describing a point
//' within the subspace. For instance an age-time
//' interaction would be a matrix, each column
//' of which represented a single time point,
//' and each row of which represented distance
//' along the c'th dimension of the subspace.
//' The matrix (or array) is implicit, in that
//' the terms themselves do not have \code{dim}
//' attributes.
//'
//' \code{dim} is the dimension of an array
//' formed by combining all the non-age (or non-age-sex)
//' dimensions in \code{terms}. For instance, if the
//' elements of \code{terms} represent an age effect,
//' an age-time interaction, and an age-region
//' interaction, then the (implicit) array
//' has two dimensions: time and region.
//'
//' Each element of \code{mappings} gives the
//' indices of the array described above
//' that the corresponding element of \code{terms}
//' maps on to. An element of length 0
//' is used to represent a pure age (or age-sex)
//' effect.
//'
//' @param terms List of numeric vectors.
//' @param dim The dimensions of the array
//' that could be formed by all the non-age
//' dimensions.
//' @param mappings List of integer vectors,
//' the same length as \code{terms}.
//' @param b,X Numeric vector and matrix used
//' to convert back to the original space.
//'
//' @return An array, the first dimension
//' of which is age or age-sex.
//'
//' @export
[[cpp11::register]]
doubles make_age_effect(list terms,
			integers dim,
			list mappings,
			doubles b,
			cpp11::doubles_matrix<> X) {
  // extract information on inputs and outputs
  int n_term = terms.size();
  int n_dim = dim.size();
  int n_comp = X.ncol();
  int n_age = X.nrow();
  int n_nonage = 1;
  for (int i_dim = 0; i_dim < n_dim; i_dim++)
    n_nonage *= dim[i_dim];
  // return value, initially filled with 0s
  writable::doubles ans(n_age * n_nonage);
  for (int i_ans = 0; i_ans < ans.size(); i_ans++)
    ans[i_ans] = 0;
  // step through (conceptual, not actual)
  // array formed by non-age dimensions
  writable::integers pos_array(n_dim);
  for (int i_dim = 0; i_dim < n_dim; i_dim++)
    pos_array[i_dim] = 0;
  int offset_ans = 0;
  for (int i_nonage = 0; i_nonage < n_nonage; i_nonage++) {
    // assemble 'beta'
    writable::doubles beta(n_comp);
    for (int i_comp = 0; i_comp < n_comp; i_comp++)
      beta[i_comp] = 0;
    for (int i_term = 0; i_term < n_term; i_term++) {
      doubles term = terms[i_term];
      integers map = mappings[i_term];
      int n_map = map.size();
      int offset_term = 0;
      if (n_map > 0) { 
	int mult = n_comp;
	for (int i_map = 0; i_map < n_map; i_map++) {
	  int i_dim = map[i_map];
	  offset_term += pos_array[i_dim] * mult;
	  mult *= dim[i_dim];
	}
      }
      for (int i_comp = 0; i_comp < n_comp; i_comp++) {
	beta[i_comp] += term[offset_term + i_comp];
      }
    }
    // record 'X' %*% 'beta' for 'i_nonage'th
    // combination of non-age variables
    for (int i_age = 0; i_age < n_age; i_age++) {
      for (int i_comp = 0; i_comp < n_comp; i_comp++) {
	ans[offset_ans + i_age] += X(i_age, i_comp) * beta[i_comp];
      }
    }
    // add 'b' to column 'i_nonage'th  of 'ans'
    for (int i_age = 0; i_age < n_age; i_age++)
      ans[offset_ans + i_age] += b[i_age];
    // update position along the non-age dimensions
    for (int i_dim = 0; i_dim < n_dim; i_dim++) {
      if (pos_array[i_dim] < dim[i_dim] - 1) {
	pos_array[i_dim]++;
	break;
      }
      pos_array[i_dim] = 0;
    }
    offset_ans += n_age;
  }
  // convert to array and return
  writable::integers dim_ans(n_dim + 1);
  dim_ans[0] = n_age;
  for (int i = 1; i <= n_dim; i++)
    dim_ans[i] = dim[i - 1];
  ans.attr("dim") = dim_ans;
  return ans;
}


// HAS_TESTS
//' Assemble linear predictor by assembling contributions
//' of main effect and interactions
//'
//' Assemble terms that representing main effects
//' or interactions (eg age:time interactions),
//' to create linear predictor with the same
//' dimensions as the array of outcomes being modelled.
//'
//' Each element of \code{terms} is a single draw from the
//' posterior distribution for tha that term.
//'
//' \code{dim} is the dimension of the array
//' of outcomes, and hence of the linear predictor.
//'
//' Each element of \code{mappings} gives the
//' indices of the array that the corresponding
//' element of \code{terms} maps on to.
//'
//' @param terms List of numeric vectors.
//' @param dim The dimensions of the array
//' to be constructed.
//' @param mappings List of integer vectors,
//' the same length as \code{terms}.
//'
//' @return An array, with the dimension
//' specified by \code{dim}.
//'
//' @export
[[cpp11::register]]
doubles make_linear_pred(list terms,
			 integers dim,
			 list mappings) {
  // extract information on inputs and outputs
  int n_term = terms.size();
  int n_dim = dim.size();
  int n_ans = 1;
  for (int i_dim = 0; i_dim < n_dim; i_dim++) {
    n_ans *= dim[i_dim];
  }
  writable::doubles ans(n_ans);
  for (int i_ans = 0; i_ans < n_ans; i_ans++) {
    ans[i_ans] = 0;
  }
  writable::integers pos_ans(n_dim);
  for (int i_dim = 0; i_dim < n_dim; i_dim++) {
    pos_ans[i_dim] = 0;
  }
  // step through 'ans'
  for (int i_ans = 0; i_ans < n_ans; i_ans++) {
    // add contribution from each element of 'terms'
    for (int i_term = 0; i_term < n_term; i_term++) {
      doubles term = terms[i_term];
      integers map = mappings[i_term];
      int n_map = map.size();
      int offset_term = 0;
      if (n_map > 0) {
	int mult = 1;
	for (int i_map = 0; i_map < n_map; i_map++) {
	  int i_dim = map[i_map];
	  offset_term += pos_ans[i_dim] * mult;
	  mult *= dim[i_dim];
	}
      }
      ans[i_ans] += term[offset_term];
    }
    // update 'pos_ans'
    for (int i_dim = 0; i_dim < n_dim; i_dim++) {
      if (pos_ans[i_dim] < dim[i_dim] - 1) {
	pos_ans[i_dim]++;
	break;
      }
      pos_ans[i_dim] = 0;
    }
  }
  // convert to array and return
  ans.attr("dim") = dim;
  return ans;
}
