% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bage_mod-methods.R
\name{is_fitted}
\alias{is_fitted}
\title{Test Whether a Model has Been Fitted}
\usage{
is_fitted(x)
}
\arguments{
\item{x}{An object of class \code{"bage_mod"}.}
}
\value{
\code{TRUE} or \code{FALSE}
}
\description{
Test whether \link[=fit.bage_mod]{fit()} has been
called on a model object.
}
\examples{
mod <- mod_pois(injuries ~ age + sex + year,
                data = nzl_injuries,
                exposure = popn)
is_fitted(mod)
mod <- fit(mod)
is_fitted(mod)
}
\seealso{
\itemize{
\item \code{\link[=mod_pois]{mod_pois()}}, \code{\link[=mod_binom]{mod_binom()}}, \code{\link[=mod_norm]{mod_norm()}} to specify a model
\item \code{\link[=fit]{fit()}} to fit a model
}
}
