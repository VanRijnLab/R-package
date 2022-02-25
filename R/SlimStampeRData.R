#' SlimStampeRData: Analyzing SlimStampen App Data
#'
#' The SlimStampeRData package provides three categories of important functions:
#' basic data descriptions, plotting column information and plotting calculated
#' information.
#'
#' @section Basic data descriptions:
#' These functions...
#'
#' @docType package
#' @name SlimStampeRData
NULL
#> NULL

## Hacky solution to R CMD Check 'no visible binding for global variable' note
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("factId", "userId", "sessionTime", "reactionTime", "correct", "alpha", "lessonTitle", "sessionId",
                           "factText", "repetition", "fact_id", "time", "mean_alpha", "mean_RT", "mean_accuracy"))
}

