% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bage_prior-constructors.R
\name{RWSeas}
\alias{RWSeas}
\title{Random Walk Prior, with Seasonal Effect}
\usage{
RWSeas(n, s = 1, s_seas = 1, along = NULL)
}
\arguments{
\item{n}{Number of seasons}

\item{s}{Scale for prior for innovations in
the random walk. Default is \code{1}.}

\item{s_seas}{Scale for prior for innovations
in the seasonal effect. Default is \code{1}.
Can be \code{0}.}

\item{along}{Name of the variable to be used
as the "along" variable. Only used with
interactions.}
}
\value{
Object of class \code{"bage_prior_rwseasvary"}
or \code{"bage_prior_rwseasfix"}.
}
\description{
Prior for a main effect or interaction,
typically involving time. Combines
a random walk with a seasonal effect.
}
\details{
If \code{RWSeas()} is used with an interaction,
then separate series are used for
the "along" variable within
each combination of the
"by" variables.

Argument \code{s} controls the size of innovations in the random walk.
Smaller values for \code{s} tend to give smoother series.

Argument \code{n} controls the number of \code{seasons}.
When using quarterly data, for instance,
\code{n} should be \code{4}, and when using
monthly data, \code{n} should be \code{12}.

By default, the magnitude of seasonal effects
can change over time. However, setting \code{s_seas}
to \code{0} produces seasonal effects that are "fixed",
ie that are the same every year.
}
\section{Mathematical details}{


When \code{RWSeas()} is used with a main effect,

\deqn{\beta_j = \alpha_j + \lambda_j}
\deqn{\alpha_j \sim \text{N}(\alpha_{j-1}, \tau^2)}
\deqn{\lambda_j \sim \text{N}(\lambda_{j-n}, \omega^2),}

and when it is used with an interaction,

\deqn{\beta_{u,v} = \alpha_{u,v} + \lambda_{u,v}}
\deqn{\alpha_{u,v} \sim \text{N}(\alpha_{u,v-1}, \tau^2),}
\deqn{\lambda_{u,v} \sim \text{N}(\lambda_{u,v-n}, \omega^2)}

where
\itemize{
\item \eqn{\pmb{\beta}} is the main effect or interaction;
\item \eqn{\alpha_j} or \eqn{\alpha_{u,v}} is an element of the random walk;
\item \eqn{\lambda_j} or \eqn{\lambda_{u,v}} is an element of the seasonal effect;
\item \eqn{j} denotes position within the main effect;
\item \eqn{v} denotes position within the "along" variable of the interaction;
\item \eqn{u} denotes position within the "by" variable(s) of the interaction; and
\item \eqn{n} is the number of seasons.
}

Parameter \eqn{\omega} has a half-normal prior
\deqn{\omega \sim \text{N}^+(0, \text{s\_seas}^2),}
where \code{s_seas} is provided by the user. If
\code{s_seas} is set to 0, then \eqn{\omega} is 0,
and the seasonal effects are fixed over time.

Parameter \eqn{\tau} has a half-normal prior
\deqn{\tau \sim \text{N}^+(0, \text{s}^2),}
where \code{s} is provided by the user.
}

\examples{
RWSeas(n = 4)             ## seasonal effects evolve
RWSeas(n = 4, s_seas = 0) ## seasonal effects fixed
}
\seealso{
\itemize{
\item \code{\link[=RW]{RW()}} Random walk without seasonal effect
\item \code{\link[=RW2Seas]{RW2Seas()}} Random walk with drift, with seasonal effect
\item \link{priors} Overview of priors implemented in \strong{bage}
\item \code{\link[=set_prior]{set_prior()}} Specify prior for intercept,
main effect, or interaction
}
}
