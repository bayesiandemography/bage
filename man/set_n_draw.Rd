% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bage_mod-functions.R
\name{set_n_draw}
\alias{set_n_draw}
\title{Set the number of draws}
\usage{
set_n_draw(mod, n_draw = 1000L)
}
\arguments{
\item{mod}{A \code{bage_mod} object, typically
created with \code{\link[=mod_pois]{mod_pois()}},
\code{\link[=mod_binom]{mod_binom()}}, or \code{\link[=mod_norm]{mod_norm()}}.}

\item{n_draw}{Number of draws.}
}
\value{
A \code{bage_mod} object
}
\description{
Specify the number of draws from the posterior
distribution to be used in model output.
A newly-created \code{bage_mod} object has an
\code{n_draw} value of 1000. Higher values
may be appropriate for characterising
the tails of distributions, or for
publication-quality graphics and summaries.
}
\details{
The value of \code{n_draw} does not affect
model fitting: it only affects posterior
summaries.
}
\examples{
mod <- mod_pois(injuries ~ age:sex + ethnicity + year,
                data = injuries,
                exposure = popn)
mod

mod |>
  set_n_draw(n_draw = 5000)
}
\seealso{
Functions whose output is affected
by the number of draws include \code{\link[=augment]{augment()}}.
}
