% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/priors.R
\name{priors}
\alias{priors}
\title{Priors for Intercept, Main Effects, Interactions}
\description{
The models created with functions \code{\link[=mod_pois]{mod_pois()}}, \code{\link[=mod_binom]{mod_binom()}},
and \code{\link[=mod_norm]{mod_norm()}} always include an intercept,
and typically include main effects
and interactions formed from variables in input data.
Most models, for instance
include an age effect, and many include
an interaction between age and sex/gender,
or age and time.

The intercept, main effects, and interactions
all have prior models that capture the expected
behavior of the term. Current choices of prior
models are summarised here.
}
\details{
\tabular{llll}{
   \strong{Prior} \tab \strong{Description} \tab \strong{Uses} \tab \strong{Forecast} \cr
   \code{\link[=N]{N()}} \tab Elements drawn from normal distribution \tab Term with no natural order \tab Yes \cr
   \code{\link[=NFix]{NFix()}} \tab As for \code{N()}, but standard deviation fixed \tab Term with few elements \tab Yes \cr
   \code{\link[=RW]{RW()}} \tab Random walk \tab Smoothing \tab Yes \cr
   \code{\link[=RW2]{RW2()}} \tab Second-order random walk \tab Like \code{RW()}, but smoother \tab Yes \cr
   \code{\link[=RW_Seas]{RW_Seas()}} \tab Random walk, with seasonal effect \tab Terms involving time \tab Yes \cr
   \code{\link[=RW2_Seas]{RW2_Seas()}} \tab Second-order random walk, with seasonal effect \tab Term involving time \tab Yes \cr
   \code{\link[=AR]{AR()}} \tab Auto-regressive prior of order \emph{k} \tab Mean reversion \tab Yes \cr
   \code{\link[=AR1]{AR1()}} \tab Auto-regressive prior of order 1 Special case of \code{AR()} \tab Mean reversion \tab Yes \cr
   \code{\link[=Known]{Known()}} \tab Values  treated as known \tab Simulations,  prior knowledge \tab No \cr
   \code{\link[=Lin]{Lin()}} \tab Linear trend, with independent normal \tab Parsimonious model for time \tab Yes \cr
   \code{\link[=Lin_AR]{Lin_AR()}} \tab Linear trend, with autoregressive errors \tab Term involving time \tab Yes \cr
   \code{\link[=Lin_AR1]{Lin_AR1()}} \tab Linear trend, with AR1 errors \tab Terms involving time \tab Yes \cr
   \code{\link[=Sp]{Sp()}} \tab P-Spline (penalised spline) \tab Smoothing, eg over age \tab No \cr
   \code{\link[=SVD]{SVD()}} \tab Age or age-sex profile based on SVD of database \tab Age or age-sex \tab No \cr
   \code{\link[=SVD_AR]{SVD_AR()}} \tab \code{SVD()}, but coefficients follow \code{AR()} \tab Age or age-sex and time \tab Yes \cr
   \code{\link[=SVD_AR1]{SVD_AR1()}} \tab \code{SVD()}, but coefficients follow \code{AR1()} \tab Age or age-sex and time \tab Yes \cr
   \code{\link[=SVD_RW]{SVD_RW()}} \tab \code{SVD()}, but coefficients follow \code{RW()} \tab Age or age-sex and time \tab Yes \cr
   \code{\link[=SVD_RW2]{SVD_RW2()}} \tab \code{SVD()}, but coefficients follow \code{RW2()} \tab Age or age-sex and time \tab Yes \cr
}
}
