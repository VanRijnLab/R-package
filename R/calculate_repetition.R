#' Calculate fact repetition
#'
#' Returns the data frame with a column for fact repetition for all trials. This
#' will override existing columns called 'repetition'. This data frame will not
#' contain any data that has been reset.
#'
#' @family calculation functions
#'
#' @param data A data frame
#'
#' @return Original data frame with column for repetition, without reset data.
#' @export
#'
calculate_repetition <- function(data) {
  if(missing(data)){
    stop("No data is provided")
  }
  missingcol <- missing_columns_check(data, c("sessionId", "factId", "sessionTime", "lessonId", "userId", "presentationStartTime"))
  if(length(missingcol) > 0){
    stop("No ", missingcol[[1]] ," column is provided in the data")
  }

  if(-1 %in% data$factId){
    data <- resetremoval(data)
    cat("- There are resets present in the data. Reset data is removed. - \n")
  }

  cat("This may take a moment... \n")

  groupdata <- dplyr::group_by(data, lessonId, userId, factId)
  sortdata <- dplyr::arrange(groupdata, presentationStartTime, .by_group = TRUE)
  mutatedata <- dplyr::mutate(sortdata, repetition = 0:(dplyr::n()-1))
  totaldata <- dplyr::arrange(mutatedata, presentationStartTime)

  cat("Done! \n")
  return(totaldata)

}
