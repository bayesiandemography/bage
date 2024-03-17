
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
#' @section Ordinary Priors:
#' 
#' The intercept, main effects, and interactions
#' all have prior models that capture the expected
#' behavior of the term. Current choices of prior
#' models are summarised here.
#'
#' Priors with links to help have been implemented.
#' Priors without links are next in the queue for implementation.
#'
#' | **Prior** |   **Description**        | **Uses**                     |
#' |-----------|--------------------------|------------------------------|
#' | [N()]     | Elements drawn from a common normal distribution with mean 0 and standard devation estimated from the data. | Main effects or interactions with no natural order or neighbourbood structure. |
#' | [NFix()]  | As for `N()`, but standard deviation supplied (fixed) by user. | Like `N()`, but where there are too few elements to estimate the standard deviation, or where there is strong prior information about its value. Intercept and sex/gender effects. |
#' | [RW()]    | Random walk. Elements have a natural ordering, and neighbouring elements more strongly correlated than distant elements. | Main effects for age, time, cohort, or other smoothly-varying dimensions. |
#' | [RW2()]   | Second-order random walk. Similar to `RW()`, but with persistent trends. | As for `RW()`, but where greater smoothness expected. |
#' | [AR()]    | Auto-regressive prior of order *k*. | Main effects for time, or for other variables that revert towards the mean. |
#' | [AR1()]   | Auto-regressive prior of order 1. Special case of `AR()`, though has more options for controlling damping. | As for `AR()`. |
#' | [Known()] | Values supplied by treated and treated as known. | Simulations and experiments. |
#' | [Lin()]   | Linear trend, with idiosyncratic errors. | Simple model for time main effect. |
#' | [Sp()]    | P-Spline (penalised spline). | Main effects that vary smoothly, eg age. |
#' | [SVD()]   | Age profile that looks like it was drawn at random from a database of rates, which are summarised using a singular value decomposition (SVD). | Age main effect. |
#' | [SVDS()]  | Age-sex or age-gender profile, similar to `SVD()` | Age-sex or age-gender interaction. |
#' | [ERW()]   | Exchangeable random walk. A random walk within each combination of the 'by' variables. | Age, time, or cohort interacted with other variable(s). |
#' | `ERW2()`  | Exchangeable second-order random walk. | As for `ERW()`. |
#' | [EAR()]   | Exchangeable auto-regressive prior. An AR model within each combination of the 'by' variables. | Age, time, or cohort interacted with other variable(s). |
#' | [EAR1()]  | Exchangeable auto-regressive prior of order 1. Special case of `EAR()`, but with more options for controlling dampling. | Age, time, or cohort interacted with other variable(s). |
#' | [ELin()]  | Linear model trend, with idiosyncratic error, within each combination of the 'by' variables. | Interaction between time and other variables. |
#' | [ESVD()]  | `SVD()` prior within each combination of the 'by' variables. | Interaction between age and non-sex, non-gender variabes. |
#' | [ESVDS()] | `SVDS()` prior within each combination of the 'by' variables. | Interaction between age, sex/gender, and one or more other variables. |
#' | `ESp()`   | Exchangeable P-Spline. `Sp()` within each combination of the 'by' variables. | Interaction between smooth variable and other variables. |
#'
#'
#' @section Composite Priors:
#'
#' Time main effects, and interactions involving time, can be modelled
#' using a prior built up from several components. A composite time
#' prior is specified using [compose_time()]. The elements of the
#' prior are as follows. All elements other than `trend` are optional.
#'
#' | Term       | Main effects                     | Interactions      |
#' |------------|----------------------------------|-------------------|
#' | `trend`    | [Lin()], [RW()], [RW2()], [Sp()] | [ELin()]          |
#' | `seasonal` | [Seas()]                         | [ESeas()]         |
#' | `cyclical` | [AR()], [AR1()]                  | [EAR()], [EAR1()] |
#' | `error`    | [N()]                            | [N()]             |
#' 
#' 
#' @name doc-priors
NULL
