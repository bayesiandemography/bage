
#' Priors for Intercept, Main Effects, Interactions
#'
#' The models for rates, probabilities, or means
#' created with functions [mod_pois()], [mod_binom()],
#' and [mod_norm()] always include an intercept,
#' and typically include main effects,
#' and interactions. Most models, for instance
#' include an age effect, and many include
#' and interaction between age and sex/gender,
#' or age and time.
#'
#' The intercept, main effects, and interactions
#' all have prior models that capture the expected
#' behavior of the term. Current choices of prior
#' models are summarised here.
#'
#' Priors with links to help have been implemented.
#' Priors without links are next in the queue for implementation.
#'
#' | **Prior** |   **Description**        | **Uses**                     | **Can be forecasted** |
#' |-----------|--------------------------|------------------------------|------|
#' | [N()]     | Elements drawn from a common normal distribution with mean 0 and standard devation estimated from the data. | Main effects or interactions with no natural order or neighbourbood structure. | Yes |
#' | [NFix()]  | As for `N()`, but standard deviation supplied (fixed) by user. | Like `N()`, but where there are too few elements to estimate the standard deviation, or where there is strong prior information about its value. Intercept and sex/gender effects. | Yes |
#' | [RW()]    | Random walk. Elements have a natural ordering, and neighbouring elements more strongly correlated than distant elements. | Modelling age, time, cohort, or other smoothly-varying dimensions. | Yes |
#' | [RW2()]   | Second-order random walk. Similar to `RW()`, but with persistent trends. | As for `RW()`, but where greater smoothness expected. | Yes |
#' | [RW_Seas()] | Random walk, with seasonal effect. | Main effects or interactions involving time. | Yes |
#' | [RW2_Seas()] | Random walk with drift, with seasonal effect. | Main effects or interactions involving time. | Yes |
#' | [AR()]    | Auto-regressive prior of order *k*. | Modelling variables that revert towards the mean, which often includes time. | Yes |
#' | [AR1()]   | Auto-regressive prior of order 1. Special case of `AR()`, though has more options for controlling damping. | As for `AR()`. | Yes |
#' | [Known()] | Values supplied by treated and treated as known. | Simulations and experiments. | No |
#' | [Lin()]   | Linear trend, with independent normal. | Parsimonious model for time. | Yes |
#' | [Lin_AR()]   | Linear trend, with autoregressive errors. | Model for time, where departures from trend persist over time. | Yes |
#' | [Lin_AR1()]   | Linear trend, with autoregressive errors of order 1. | Simpler version of `Lin_AR()`. | Yes |
#' | [Sp()]    | P-Spline (penalised spline). | Modelling varies that smoothly, eg age. | No |
#' | [SVD()]   | Age, age-sex, or age-gender profile that looks like it was drawn at random from a database of rates, which are summarised using a singular value decomposition (SVD). | Terms involving age and possibly sex/gender. | No |
#' | [SVD_AR()] | Like `SVD()`, but coefficients evolve over time, following AR models. | Interactions between age, time, possibly sex/gender, and possibly other variables. | Yes |
#' | [SVD_AR1()] | Like `SVD()`, but coefficients evolve over time, following AR1 models. | Interactions between age, time, possibly sex/gender, and possibly other variables. | Yes |
#' | [SVD_RW()] | Like `SVD()`, but coefficients evolve over time, following random walk models. | Interactions between age, time, possily sex/gender, and possibly other variables. | Yes |
#' | [SVD_RW2()] | Like `SVD()`, but coefficients evolve over time, following random walk with drift models. | Interactions between age, time, possibly sex/gender, and possibly other variables. | Yes |
#'
#' @name priors
NULL
