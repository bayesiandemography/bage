% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bage_mod-methods.R
\name{augment.bage_mod}
\alias{augment.bage_mod}
\title{Extract Data and Modelled Values}
\usage{
\method{augment}{bage_mod}(x, quiet = FALSE, ...)
}
\arguments{
\item{x}{An object of class \code{"bage_mod"}.}

\item{quiet}{Whether to suppress messages.
Default is \code{FALSE}.}

\item{...}{Unused. Included for generic consistency only.}
}
\value{
A \link[tibble:tibble-package]{tibble}, with the original
data plus the following columns:
\itemize{
\item \code{.observed} 'Direct' estimates of rates or
probabilities, ie counts divided by exposure or size
(in Poisson and binomial models.)
\item \code{.fitted} Draws of rates, probabilities,
or means.
\item \code{.expected} Draws of expected values for
rates or probabilities (in Poisson
that include exposure, or in binomial models.)
}

Uncertain quantities are represented using
\link[rvec:rvec]{rvecs}.
}
\description{
Extract data and rates, probabilities, or means
from a model object.
The return value consists of the original
data and one or more columns of modelled values.
}
\section{Fitted vs unfitted models}{


\code{augment()} is typically called on a \link[=fit]{fitted}
model. In this case, the modelled values are
draws from the joint posterior distribution for rates,
probabilities, or means.

\code{augment()} can, however, be called on an
unfitted model. In this case, the modelled values
are draws from the joint prior distribution.
In other words, the modelled values are informed by
model priors, and by values for \code{exposure}, \code{size}, or \code{weights},
but not by observed outcomes.
}

\examples{
## specify model
mod <- mod_pois(injuries ~ age + sex + year,
                data = injuries,
                exposure = popn)

## look at prior distribution
mod |> augment()

## fit model
mod <- mod |>
  fit()

## look at posterior distribution
mod |> augment()
}
\seealso{
\itemize{
\item \code{\link[=components]{components()}} Extract values for hyper-parameters from a model
\item \code{\link[=tidy]{tidy()}} Extract a one-line summary of a model
\item \code{\link[=mod_pois]{mod_pois()}} Specify a Poisson model
\item \code{\link[=mod_binom]{mod_binom()}} Specify a binomial model
\item \code{\link[=mod_norm]{mod_norm()}} Specify a normal model
\item \code{\link[=fit]{fit()}} Fit a model
\item \code{\link[=is_fitted]{is_fitted()}} See if a model has been fitted
}
}
