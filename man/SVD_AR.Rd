% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bage_prior-constructors.R
\name{SVD_AR}
\alias{SVD_AR}
\alias{SVD_AR1}
\alias{SVD_RW}
\alias{SVD_RW2}
\title{Dynamic SVD-Based Priors for Age Profiles or Age-Sex Profiles}
\usage{
SVD_AR(ssvd, n_comp = NULL, indep = TRUE, n_coef = 2, s = 1)

SVD_AR1(ssvd, n_comp = NULL, indep = TRUE, min = 0.8, max = 0.98, s = 1)

SVD_RW(ssvd, n_comp = NULL, indep = TRUE, s = 1)

SVD_RW2(ssvd, n_comp = NULL, indep = TRUE, s = 1)
}
\arguments{
\item{ssvd}{Object of class \code{"bage_ssvd"}
holding a scaled SVD. See below for scaled SVDs
of databases currently available in \strong{bage}.}

\item{n_comp}{Number of components from scaled SVD
to use in modelling. The default is half
the number of components of \code{ssvd}.}

\item{indep}{Whether to use separate or
combined SVDs in terms involving sex or gender.
Default is \code{TRUE}.
See below for details.}

\item{n_coef}{Number of AR coefficients in \code{SVD_RW()}.}

\item{s}{Scale for standard deviations terms.}

\item{min, max}{Minimum and maximum values
for autocorrelation coefficient in \code{SVD_AR()}.
Defaults are \code{0.8} and \code{0.98}.}
}
\value{
An object of class \code{"bage_prior_svd_ar"},
\code{"bage_prior_svd_rw"}, or \code{"bage_prior_svd_rw2"}.
}
\description{
Use components from a Singular Value Decomposition (SVD)
to model an interaction involving age and time, or age,
sex/gender and time, where the coefficients evolve over time.
}
\details{
\code{SVD_AR()}, \code{SVD_AR1()}, \code{SVD_RW()}, and \code{SVD_RW2()}
priors assume that, in any given period,
the age profiles or age-sex profiles for the quantity
being modelled looks like they were drawn at random
from an external demographic database. For instance,
the \code{SVD_AR()} prior obtained via

\if{html}{\out{<div class="sourceCode">}}\preformatted{SVD_AR(HMD)
}\if{html}{\out{</div>}}

assumes that profiles look like
they were obtained from the
\href{https://www.mortality.org}{Human Mortality Database}.
}
\section{Mathematical details}{


When the interaction being modelled only involves
age and time, or age, sex/gender, and time

\deqn{\pmb{\beta}_t = \pmb{F} \pmb{\alpha}_t + \pmb{g},}

and when it involves other variables besides age, sex/gender, and time,

\deqn{\pmb{\beta}_{u,t} = \pmb{F} \pmb{\alpha}_{u,t} + \pmb{g},}

where
\itemize{
\item \eqn{\pmb{\beta}} is an interaction involving age, time, possibly sex/gender,
and possibly other variables;
\item \eqn{\pmb{\beta}_t} is a subvector of \eqn{\pmb{\beta}} holding
values for period \eqn{t};
\item \eqn{\pmb{\beta}_{u,t}} is a subvector of \eqn{\pmb{\beta}_t} holding
values for the  \eqn{u}th combination of the non-age, non-time,
non-sex/gender variables for period \eqn{t};
\item \eqn{J} is the number of elements of \eqn{\pmb{\beta}_t};
\item \eqn{V} is the number of elements of \eqn{\pmb{\beta}_{u,t}};
\item \eqn{n} is \code{n_coef};
\item \eqn{\pmb{F}} is a known matrix with dimension \eqn{J \times n}
or \eqn{V \times n};
\item \eqn{\pmb{g}} is a known vector with \eqn{J} or \eqn{V}
elements.
}

\eqn{\pmb{F}} and \eqn{\pmb{g}} are constructed from
a large database of age-specific demographic estimates
by performing an SVD and standardizing.

With \code{SVD_AR()}, the prior for the \eqn{k}th element
of \eqn{\pmb{\alpha}_t} or \eqn{\pmb{\alpha}_{u,t}} is

\deqn{\alpha_{k,t} = \phi_1 \alpha_{k,t-1} + \cdots + \phi_n \beta_{k,t-n} + \epsilon_{k,t}}

or

\deqn{\alpha_{k,u,t} = \phi_1 \alpha_{k,u,t-1} + \cdots + \phi_n \beta_{k,u,t-n} + \epsilon_{k,u,t};}

with \code{SVD_AR1()}, it is

\deqn{\alpha_{k,t} = \phi \alpha_{k,t-1} + \epsilon_{k,t}}

or

\deqn{\alpha_{k,u,t} = \phi \alpha_{k,u,t-1} + \epsilon_{k,u,t};}

with \code{SVD_RW()}, it is

\deqn{\alpha_{k,t} = \alpha_{k,t-1} + \epsilon_{k,t}}

or

\deqn{\alpha_{k,u,t} = \alpha_{k,u,t-1} + \epsilon_{k,u,t};}

and with \code{SVD_RW2()}, it is

\deqn{\alpha_{k,t} = 2 \alpha_{k,t-1} - \alpha_{k,t-2} + \epsilon_{k,t}}

or

\deqn{\alpha_{k,u,t} = 2 \alpha_{k,u,t-1} - \alpha_{k,u,t-2} + \epsilon_{k,u,t}.}

For more on the \eqn{\phi} and \eqn{\epsilon}, see \code{\link[=AR]{AR()}}, \code{\link[=AR1]{AR1()}},
\code{\link[=RW]{RW()}}, and \code{\link[=RW2]{RW2()}}.
}

\section{Scaled SVDs of demographic databases in bage}{

\itemize{
\item \code{\link{HMD}} Mortality rates from the
\href{https://www.mortality.org}{Human Mortality Database}.
\item \code{\link{LFP}} Labor forcce participation
rates from the \href{https://data-explorer.oecd.org}{OECD}.
}
}

\examples{
SVD_AR1(HMD)
SVD_RW(HMD, n_comp = 3)
SVD_RW2(HMD, indep = FALSE)
}
\references{
\itemize{
\item For details of the construction of
scaled SVDS see the vignette
\href{https://bayesiandemography.github.io/bage/articles/vig2_math.html}{here}.
}
}
\seealso{
\itemize{
\item \code{\link[=SVD]{SVD()}} SVD prior for non-time-varying terms
\item \code{\link[=RW]{RW()}} Smoothing via random walk
\item \code{\link[=RW2]{RW2()}} Smoothing via second-order random walk
\item \code{\link[=Sp]{Sp()}} Smoothing via splines
\item \link{priors} Overview of priors implemented in \strong{bage}
\item \code{\link[=set_prior]{set_prior()}} Specify prior for intercept,
main effect, or interaction
\item \code{\link[=set_var_sexgender]{set_var_sexgender()}} Identify sex or gender variable in data
}
}
