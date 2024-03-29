% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bage_mod-constructors.R
\name{mod_binom}
\alias{mod_binom}
\title{Specify a binomial model}
\usage{
mod_binom(formula, data, size)
}
\arguments{
\item{formula}{An R \link[stats:formula]{formula},
specifying the outcome and predictors.}

\item{data}{A data frame containing the outcome,
predictors, and, number of trials}

\item{size}{Name of the variable describing
the number of trials.}
}
\value{
An object of class \code{bage_mod}.
}
\description{
Specify a model where the outcome is drawn from
a binomial distribution.
}
\details{
\itemize{
\item \code{formula} specifies the outcome and predictors,
including interactions between predictors.
It follows standard R \link[stats:formula]{formula}
conventions, except that it cannot include
transformations (e.g. \code{sqrt(deaths)}).
\item \code{data} A data frame holding the outcome, the predictors,
and number of trials.
\item \code{size} is the name (bare or quoted) of the variable
in \code{data} measuring the number of trials.
}

The first level of the model is

\deqn{y \sim \text{binom}(n, \pi)}

where \eqn{\pi} is the sucess probability,
and \eqn{n} is the number of trials.

TODO - Include error term once specification finalised.
}
\examples{
mod <- mod_binom(oneperson ~ age:region + age:year,
                 data = households,
                 size = total)
}
\seealso{
\itemize{
\item \code{\link[=mod_pois]{mod_pois()}} and \code{\link[=mod_norm]{mod_norm()}} for specification
of Poisson and normal models
\item \code{\link[=set_prior]{set_prior()}} to specify non-default priors
\item \code{\link[=fit]{fit()}} to fit a model
}
}
