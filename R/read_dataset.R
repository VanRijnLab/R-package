#' Read dataset
#'
#' \code{read_dataset} reads the raw data set into a data frame with correct
#' settings
#' @param file A csv file. If no file is provided the example .csv file will be loaded.
#'
#' @return data frame
#' @export
#'
read_dataset <- function(file=system.file("extdata", "textcues_example.csv", package = "SlimStampeRData")) {
  if(file == system.file("extdata", "textcues_example.csv", package = "SlimStampeRData")) {
    message("Example dataset was used: textcues")
  }

  data <- data.table::fread(file)
  cols <- c("factId", "userId", "sessionTime", "reactionTime", "correct", "lessonTitle", "lessonId","sessionId",
            "factText", "alpha")
  missingcol <- missing_columns_check(data, cols)
  # MaxAlpha, MinAlpha and lookAheadTime to be added depending on function change

  if(length(missingcol) >= 0){
    stop("One or more of these columns is missing: ", cols)
  }
  return(data)
}
