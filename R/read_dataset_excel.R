#' Read data set from excel files
#'
#' \code{read_dataset_excel} reads the raw data set from multiple excel files
#' into a data frame with the correct settings
#' @param file Excel files. If no file is provided the example .xlsx file will
#'   be loaded.
#'
#' @return data frame
#' @export
#'
read_dataset_excel <- function(file_response = "../SlimStampen_data_examples/vocatrainer/nmd_live_vocatrainer_public_response.xlsx",
                               file_lesson = "../SlimStampen_data_examples/vocatrainer/nmd_live_vocatrainer_public_lesson.xlsx",
                               file_fact = "../SlimStampen_data_examples/vocatrainer/nmd_live_vocatrainer_public_fact.xlsx") {
  # if(file == system.file("extdata", "files.xlsx", package = "SlimStampeRData")) {
  #   message("Example dataset was used: excel sample")
  # }

  out <- list()

  out$dataLesson <- read_excel("~/Werk/SlimStampen/Package R/SlimStampen_data_examples/vocatrainer/nmd_live_vocatrainer_public_lesson.xlsx")

  out$dataFact <- read_excel("~/Werk/SlimStampen/Package R/SlimStampen_data_examples/vocatrainer/nmd_live_vocatrainer_public_fact.xlsx")

  out$dataResponse <- read_excel("~/Werk/SlimStampen/Package R/SlimStampen_data_examples/vocatrainer/nmd_live_vocatrainer_public_response.xlsx")





  # cols <- c("factId", "userId", "sessionTime", "reactionTime", "correct", "lessonTitle", "lessonId","sessionId",
  #           "factText")
  # missingcol <- missing_columns_check(data, cols)
  #
  # if(length(missingcol) > 0){
  #   strCols <- paste(cols,collapse=" ")
  #   warning("One or more of these columns is missing: ", strCols,"\n Some of the funcions in this package may not work if these columns are not provided")
  # }

  # # Separate messages for alpha and repetition
  # colsalpha <- c("alpha")
  # missingalpha <- missing_columns_check(data, colsalpha)
  # if(length(missingalpha) > 0){
  #   cat("! No alpha column is provided in the data, run calculate_alpha_and_activation() to add an alpha column to the data \n")
  # }
  #
  # colsrep <- c("repetition")
  # missingrep <- missing_columns_check(data, colsrep)
  # if(length(missingrep) > 0){
  #   cat("! No repetition column is provided in the data, run calculate_repetition() to add a repetition column to the data \n")
  # }


  return(out)
}
