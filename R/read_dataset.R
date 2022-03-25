#' Read data set
#'
#' \code{read_dataset} reads the raw data set into a data frame with correct
#' settings
#' @param file A .csv file. If no file is provided the example .csv file will be loaded.
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
            "factText")
  missingcol <- missing_columns_check(data, cols)

  if(length(missingcol) > 0){
    strCols <- paste(cols,collapse=" ")
    warning("One or more of these columns is missing: ", strCols,"\n Some of the funcions in this package may not work if these columns are not provided")
  }

  # Separate messages for alpha and repetition
  colsalpha <- c("alpha")
  missingalpha <- missing_columns_check(data, colsalpha)
  if(length(missingalpha) > 0){
    cat("! No alpha column is provided in the data, run calculate_alpha_and_activation() to add an alpha column to the data \n")
  }

  colsrep <- c("repetition")
  missingrep <- missing_columns_check(data, colsrep)
  if(length(missingrep) > 0){
    cat("! No repetition column is provided in the data, run calculate_repetition() to add a repetition column to the data \n")
  }


  return(data)
}
