% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bage_mod-methods.R
\name{tidy.bage_mod}
\alias{tidy.bage_mod}
\title{Summarize Terms from a Fitted Model}
\usage{
\method{tidy}{bage_mod}(x, ...)
}
\arguments{
\item{x}{A fitted \code{bage_mod} object.}

\item{...}{Unused. Included for generic consistency only.}
}
\value{
A \link[tibble:tibble-package]{tibble}.
}
\description{
Summarize the intercept, main effects, and interactions
from a fitted model.
}
\examples{
mod <- mod_pois(injuries ~ age + sex + year,
                data = injuries,
                exposure = popn)
mod <- fit(mod)
tidy(mod)
}
\seealso{
\itemize{
\item \code{\link[=augment]{augment()}} Extract data, and values for rates,
probabilities, or means
\item \code{\link[=components]{components()}} Extract values for hyper-parameters
}
}
