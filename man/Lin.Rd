% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bage_prior-constructors.R
\name{Lin}
\alias{Lin}
\title{Linear Prior with Independent Normal Errors}
\usage{
Lin(s = 1, mean_slope = 0, sd_slope = 1, along = NULL, zero_sum = FALSE)
}
\arguments{
\item{s}{Scale for the prior for the errors.
Default is \code{1}. Can be \code{0}.}

\item{mean_slope}{Mean in prior for slope
of line. Default is 0.}

\item{sd_slope}{Standard deviation in prior for slope
of line. Default is 1.}

\item{along}{Name of the variable to be used
as the 'along' variable. Only used with
interactions.}

\item{zero_sum}{If \code{TRUE}, values must
sum to 0 within each combination
of the 'by' variables. Default is \code{FALSE}.}
}
\value{
An object of class \code{"bage_prior_lin"}.
}
\description{
Use a line or lines with independent
normal errors to model a main effect
or interaction. Typically used with time.
}
\details{
If \code{Lin()} is used with an interaction,
then separate lines are constructed along
the 'along' variable, within each combination
of the 'by' variables.

Argument \code{s} controls the size of the errors.
Smaller values tend to give smoother estimates.
\code{s} can be zero.

Argument \code{sd_slope} controls the size of the slopes of
the lines. Larger values can give more steeply
sloped lines.
}
\section{Mathematical details}{


When \code{Lin()} is used with a main effect,

\deqn{\beta_j = \alpha + j \eta + \epsilon_j}
\deqn{\alpha \sim \text{N}(0, 1)}
\deqn{\epsilon_j \sim \text{N}(0, \tau^2),}

and when it is used with an interaction,

\deqn{\beta_{u,v} \sim  \alpha_u + v \eta_u + \epsilon_{u,v}}
\deqn{\alpha_u \sim \text{N}(0, 1)}
\deqn{\epsilon_{u,v} \sim \text{N}(0, \tau^2),}

where
\itemize{
\item \eqn{\pmb{\beta}} is the main effect or interaction;
\item \eqn{j} denotes position within the main effect;
\item \eqn{v} denotes position within the 'along' variable of the interaction; and
\item \eqn{u} denotes position within the 'by' variable(s) of the interaction.
}

The slopes have priors
\deqn{\eta \sim \text{N}(\text{mean_slope}, \text{sd_slope}^2)}
and
\deqn{\eta_u \sim \text{N}(\text{mean_slope}, \text{sd_slope}^2).}

Parameter \eqn{\tau} has a half-normal prior
\deqn{\tau \sim \text{N}^+(0, \text{s}^2).}
}

\examples{
Lin()
Lin(s = 0.5, sd_slope = 2)
Lin(s = 0)
Lin(along = "cohort")
}
\seealso{
\itemize{
\item \code{\link[=Lin_AR]{Lin_AR()}} Linear with AR errors
\item \code{\link[=Lin_AR1]{Lin_AR1()}} Linear with AR1 errors
\item \code{\link[=RW2]{RW2()}} Second-order random walk
\item \link{priors} Overview of priors implemented in \strong{bage}
\item \code{\link[=set_prior]{set_prior()}} Specify prior for intercept,
main effect, or interaction
}
}
