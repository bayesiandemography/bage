% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bage_prior-constructors.R
\name{AR1}
\alias{AR1}
\title{Autoregressive Prior of Order 1}
\usage{
AR1(min = 0.8, max = 0.98, s = 1, along = NULL)
}
\arguments{
\item{min, max}{Minimum and maximum values
for autocorrelation coefficient.
Defaults are \code{0.8} and \code{0.98}.}

\item{s}{Scale for the prior for the innovations.
Default is \code{1}.}

\item{along}{Name of the variable to be used
as the "along" variable. Only used with
interactions.}
}
\value{
An object of class \code{"bage_prior_ar"}.
}
\description{
Use an autoregressive process
of order 1 to model
a main effect, or use multiple AR1
processes to model an interaction.
Typically used with time effects or with
interactions that involve time.
}
\details{
If \code{AR()} is used with an interaction,
separate AR processes are constructed along
the "along" variable, within each combination of the
"by" variables.

Arguments \code{min} and \code{max} can be used to specify
the permissible range for autocorrelation.

Argument \code{s} controls the size of innovations. Smaller values
for \code{s} tend to give smoother estimates.
}
\section{Mathematical details}{


When \code{AR1()} is used with a main effect,

\deqn{\beta_j = \phi \beta_{j-1} + \epsilon_j}
\deqn{\epsilon_j \sim \text{N}(0, \omega^2),}

and when it is used with an interaction,

\deqn{\beta_{u,v} = \phi \beta_{u,v-1} + \epsilon_{u,v}}
\deqn{\epsilon_{u,v} \sim \text{N}(0, \omega^2),}

where
\itemize{
\item \eqn{\pmb{\beta}} is the main effect or interaction;
\item \eqn{j} denotes position within the main effect;
\item \eqn{v} denotes position within the "along" variable of the interaction; and
\item \eqn{u} denotes position within the "by" variable(s) of the interaction.
}

Internally, \code{AR1()} derives a value for \eqn{\omega} that
gives every element of \eqn{\beta} a marginal
variance of \eqn{\tau^2}. Parameter \eqn{\tau}
has a half-normal prior
\deqn{\tau \sim \text{N}^+(0, \text{s}^2),}
where \code{s} is provided by the user.

Coefficient \eqn{\phi} is constrained
to lie between \code{min} and \code{max}.
Its prior distribution is

\deqn{\phi = (\text{max} - \text{min}) \phi' - \text{min}}

where

\deqn{\phi' \sim \text{Beta}(2, 2).}
}

\examples{
AR1()
AR1(min = 0, max = 1, s = 2.4)
AR1(along = "cohort")
}
\references{
\itemize{
\item \code{AR1()} is based on the TMB function
\href{http://kaskr.github.io/adcomp/classdensity_1_1AR1__t.html#details}{AR1}
\item The defaults for \code{min} and \code{max} are based on the
defaults for \code{forecast::ets()}.
}
}
\seealso{
\itemize{
\item \code{\link[=AR]{AR()}} Generalization of \code{AR1()}
\item \code{\link[=Lin_AR]{Lin_AR()}}, \code{\link[=Lin_AR1]{Lin_AR1()}} Line with AR errors
\item \link{priors} Overview of priors implemented in \strong{bage}
\item \code{\link[=set_prior]{set_prior()}} Specify prior for intercept,
main effect, or interaction
}
}
