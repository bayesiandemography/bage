% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bage_mod-constructors.R
\name{mod_binom}
\alias{mod_binom}
\title{Specify a Binomial Model}
\usage{
mod_binom(formula, data, size)
}
\arguments{
\item{formula}{An R \link[stats:formula]{formula},
specifying the outcome and predictors.}

\item{data}{A data frame containing the outcome
and predictor variables, and the number of trials.}

\item{size}{Name of the variable giving
the number of trials, or a formula.}
}
\value{
An object of class \code{bage_mod}.
}
\description{
Specify a model where the outcome is drawn from
a binomial distribution.
}
\details{
The model is hierarchical. The probabilities in the binomial distribution
are described by a prior model formed from dimensions such
as age, sex, and time. The terms for these dimension themselves
have models, as described in \link{priors}. These priors all have defaults,
which depend on the type of term (eg an intercept, an age main effect,
or an age-time interaction.)
}
\section{Specifying size}{


The \code{size} argument can take two forms:
\itemize{
\item the name of a variable in \code{data}, with or without
quote marks, eg \code{"population"} or \code{population}; or
\item a formula, which is evaluated with \code{data} as its
environment (see below for example).
}
}

\section{Mathematical details}{


The likelihood is

\deqn{y_i \sim \text{binomial}(\gamma_i; w_i)}

where
\itemize{
\item subscript \eqn{i} identifies some combination of the the
classifying variables, such as age, sex, and time;
\item \eqn{y_i} is a count, such of number of births,
such as age, sex, and region;
\item \eqn{\gamma_i} is a probability of 'success'; and
\item \eqn{w_i} is the number of trials.
}

The probabilities \eqn{\gamma_i} are assumed to be drawn
a beta distribution

\deqn{y_i \sim \text{Beta}(\xi^{-1} \mu_i, \xi^{-1} (1 - \mu_i))}

where
\itemize{
\item \eqn{\mu_i} is the expected value for \eqn{\gamma_i}; and
\item \eqn{\xi} governs dispersion (ie variance.)
}

Expected value \eqn{\mu_i} equals, on a logit scale,
the sum of terms formed from classifying variables,

\deqn{\text{logit} \mu_i = \sum_{m=0}^{M} \beta_{j_i^m}^{(m)}}

where
\itemize{
\item \eqn{\beta^{0}} is an intercept;
\item \eqn{\beta^{(m)}}, \eqn{m = 1, \dots, M}, is a main effect
or interaction; and
\item \eqn{j_i^m} is the element of \eqn{\beta^{(m)}} associated with
cell \eqn{i}.
}

The \eqn{\beta^{(m)}} are given priors, as described in \link{priors}.

\eqn{\xi} has an exponential prior with mean 1. Non-default
values for the mean can be specified with  \code{\link[=set_disp]{set_disp()}}.

The model for \eqn{\mu_i}
can also include covariates,
as described in \code{\link[=set_covariates]{set_covariates()}}.
}

\examples{
mod <- mod_binom(oneperson ~ age:region + age:year,
                 data = nzl_households,
                 size = total)

## use formula to specify size
mod <- mod_binom(ncases ~ agegp + tobgp + alcgp,
                 data = esoph,
                 size = ~ ncases + ncontrols)
}
\seealso{
\itemize{
\item \code{\link[=mod_pois]{mod_pois()}} Specify Poisson model
\item \code{\link[=mod_norm]{mod_norm()}} Specify normal model
\item \code{\link[=set_prior]{set_prior()}} Specify non-default prior for term
\item \code{\link[=set_disp]{set_disp()}} Specify non-default prior for dispersion
\item \code{\link[=fit]{fit()}} Fit a model
\item \code{\link[=augment]{augment()}} Extract values for probabilities,
together with original data
\item \code{\link[=components]{components()}} Extract values for hyper-parameters
\item \code{\link[=forecast]{forecast()}} Forecast parameters and outcomes
\item \code{\link[=report_sim]{report_sim()}} Check model using simulation study
\item \code{\link[=replicate_data]{replicate_data()}} Check model using replicate data
\item \href{https://bayesiandemography.github.io/bage/articles/vig2_math.html}{Mathematical Details}
Detailed descriptions of models
}
}
