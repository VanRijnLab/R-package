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

  data.table::fread(file)
}
