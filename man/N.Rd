% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bage_prior-constructors.R
\name{N}
\alias{N}
\title{Normal Prior}
\usage{
N(s = 1)
}
\arguments{
\item{s}{Scale for the standard deviation.
Default is \code{1}.}
}
\value{
An object of class \code{"bage_prior_norm"}.
}
\description{
Use independent draws from a normal
distribution to model a main effect or interaction.
Typically used with variables other than
age or time, such as region or ethnicity,
where there is no natural ordering.
}
\details{
Argument \code{s} controls the size of errors. Smaller values
for \code{s} tend to give more tightly clustered estimates.
}
\section{Mathematical details}{


\deqn{\beta_j \sim \text{N}(0, \tau^2)}

where \eqn{\beta} is the main effect or interaction.

Parameter \eqn{\tau}
has a half-normal prior
\deqn{\tau \sim \text{N}^+(0, \mathtt{s}^2),}
where \code{s} is provided by the user.
}

\examples{
N()
N(s = 0.5)
}
\seealso{
\itemize{
\item \code{\link[=NFix]{NFix()}} Similar to \code{N()} but standard deviation
parameter is supplied rather than estimated from data
\item \link{priors} Overview of priors implemented in \strong{bage}
\item \code{\link[=set_prior]{set_prior()}} Specify prior for intercept,
main effect, or interaction
\item \href{https://bayesiandemography.github.io/bage/articles/vig2_math.html}{Mathematical Details}
vignette
}
}
