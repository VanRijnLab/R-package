#' SlimStampeRData: Analyzing SlimStampen App Data
#'
#' The SlimStampeRData package provides four categories of functions: basic
#' data, individual information, average information and calculation functions.
#'
#' @section Basic data: This package includes a function to read a csv file into
#'   a data table, namely \code{read_dataset()}. This function also provides
#'   warnings if the data set does not confirm to the assumptions that the
#'   functions of this package make about the data. If no csv file is provided,
#'   an example data set is used instead. The function \code{dataset_stats()}
#'   shows global information about the data set. This includes information such
#'   as: the number of participants, number of unique facts, number of lessons,
#'   participants per lesson, and more.
#'
#' @section Individual information: There are two functions for show the data of
#'   individual participants: \code{individual_RT()} and
#'   \code{individual_ROF()}. These function make a plot for every participant
#'   (based on sessionId) that shows either the RT or ROF of all facts that the
#'   participant has seen during that session. All incorrect answers are denoted
#'   with a red marker and all correct answers are denoted with a grey marker.
#'
#' @section Average information: There are several function that shows averaged
#'   information about participants or facts. The funcitons that show the
#'   average of a variable for all participants are:
#'   \code{average_accuracy_participants()}, \code{average_RT_participants()}
#'   and \code{average_ROF_participants}. These functions show a plot in which
#'   every line in the plot represents a participant. The function that shows an
#'   average of all facts is: \code{average_ROF_facts()}, which shows the
#'   average alpha for all facts. In the plot that this function generates all
#'   the lines represent a fact.
#'
#' @section Calculation functions: Some of the functions described above need
#'   specific information to function. Functions that plot information over the
#'   amount of fact repetitions need the data to provide a column that shows the
#'   repetition for every trial. Functions that plot the alpha need the data to
#'   provide a column with the alpha for every trial. The data set that is being
#'   used may already contain these columns, but if it doesn't then there are
#'   two functions that can provide this. These functions are
#'   \code{calculate_repetition()} and \code{calculate_alpha_and_activation()}.
#'   Both these function take a data set as argument and then return the same
#'   data set with the relevant column.
#'
#'
#' @docType package
#' @name SlimStampeRData
NULL
#> NULL

## Hacky solution to R CMD Check 'no visible binding for global variable' note
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("factId", "userId", "sessionTime", "reactionTime", "correct", "alpha", "lessonTitle", "sessionId",
                           "factText", "repetition", "fact_id", "time", "mean_alpha", "mean_RT", "mean_accuracy", ".data",
                           "lessonId", "sequence_number", "presentationStartTime", "StartTime"))
}

