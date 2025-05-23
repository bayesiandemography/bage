% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bage_mod-methods.R
\name{print.bage_mod}
\alias{print.bage_mod}
\title{Printing a Model}
\usage{
\method{print}{bage_mod}(x, ...)
}
\arguments{
\item{x}{Object of class \code{"bage_mod"}, typically
created with \code{\link[=mod_pois]{mod_pois()}}, \code{\link[=mod_binom]{mod_binom()}},
or \code{\link[=mod_norm]{mod_norm()}}.}

\item{...}{Unused. Included for generic consistency only.}
}
\value{
\code{x}, invisibly.
}
\description{
After calling a function such as \code{\link[=mod_pois]{mod_pois()}} or
\code{\link[=set_prior]{set_prior()}} it is good practice to print the
model object at the console, to check the model's
structure. The output from \code{print()} has
the following components:
\itemize{
\item A header giving the class of the model
and noting whether the model has been fitted.
\item A \link[stats:formula]{formula} giving the
outcome variable and terms for the model.
\item A table giving the number of parameters, and
(fitted models only) the standard
deviation across those parameters,
a measure of the term's importance.
See \code{\link[=priors]{priors()}} and \code{\link[=tidy]{tidy()}}.
\item Values for other model settings. See \code{\link[=set_disp]{set_disp()}},
\code{\link[=set_var_age]{set_var_age()}}, \code{\link[=set_var_sexgender]{set_var_sexgender()}}, \code{\link[=set_var_time]{set_var_time()}},
\code{\link[=set_n_draw]{set_n_draw()}}
\item Details on computations (fitted models only).
See \code{\link[=computations]{computations()}}.
}
}
\examples{
mod <- mod_pois(injuries ~ age + sex + year,
                data = nzl_injuries,
                exposure = popn)

## print unfitted model
mod

mod <- fit(mod)

## print fitted model
mod
}
\seealso{
\itemize{
\item \code{\link[=mod_pois]{mod_pois()}} Specify a Poisson model
\item \code{\link[=mod_binom]{mod_binom()}} Specify a binomial model
\item \code{\link[=mod_norm]{mod_norm()}} Specify a normal model
\item \link[=fit]{fit.bage_mod()} and \code{\link[=is_fitted]{is_fitted()}} Model fitting
\item \link{priors} Overview of priors for model terms
\item \link[=tidy]{tidy.bage_mod()} Number of parameters,
and standard deviations
\item \code{\link[=set_disp]{set_disp()}} Dispersion
\item \code{\link[=set_var_age]{set_var_age()}}, \code{\link[=set_var_sexgender]{set_var_sexgender()}}, \code{\link[=set_var_time]{set_var_time()}}
Age, sex/gender and time variables
\item \code{\link[=set_n_draw]{set_n_draw()}} Model draws
}
}
