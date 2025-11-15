
#' Scaled SVDs
#'
#' @description 
#' Scaled SVDs contain information on typical age-patterns
#' or age-sex patterns for demographic processes,
#' extracted from international databases. The
#' information is extracted using a singular
#' value decomposition (SVD), and then scaled to make
#' it easier to formulate priors.
#'
#' Scaled SVDs can have multiple versions, based
#' on data released at different dates, or on
#' subsets of the available data.
#'
#' Some datasets, and hence some scaled SVDs, include information
#' on age but not on sex or gender.
#'
#' @details
#'
#' | **Scaled SVD** | **Process**                              | **Source**                   | **Versions**                    | **Sex dimension** |
#' |----------------|------------------------------------------|-----------------------------------|---------------------------------|---------|
#' | [CSA]          | School attendance                        | Census data assembled by UN       | `"v2025"`, `"v2024"`            | Yes      |
#' | [HFD]          | Fertility                                | Human Fertility Database          | `"v2025"`, `"v2024"`            | No      |
#' | [HIMD_R]       | Internal migration: annual rates         | Human Internal Migration Database | `"v2024"`                       | No      |
#' | [HIMD_P1]      | Internal migration: 1-year probabilities | Human Internal Migration Database | `"v2024"`                       | No      |
#' | [HIMD_P5]      | Internal migration: 5-year probabilities | Human Internal Migration Database | `"v2024"`                       | No      |
#' | [HMD]          | Mortality                                | Human Mortality Database          | `"v2025"`, `"v2025_50"`, `"v2024"`| Yes     |
#' | [LFP]          | Labour Force Participation               | OECD                              | `"v2025"`                       | Yes     |
#' | [WMD_C]        | Currently married                        | World Migration Data              | `"v2019"`                       | Yes     |
#' | [WMD_E]        | Ever married                             | World Migration Data              | `"v2019"`                       | Yes     |
#' 
#' @name svds
NULL
