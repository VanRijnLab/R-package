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
read_dataset_excel <- function(file_response = "../SlimStampen_data_examples/vocatrainer/small_response.xlsx",
                               file_lesson = "../SlimStampen_data_examples/vocatrainer/nmd_live_vocatrainer_public_lesson.xlsx",
                               file_fact = "../SlimStampen_data_examples/vocatrainer/nmd_live_vocatrainer_public_fact.xlsx") {
  # if(file == system.file("extdata", "files.xlsx", package = "SlimStampeRData")) {
  #   message("Example dataset was used: excel sample")
  # }
  cat("This may take a few minutes... \n")

  out <- list()

  out$dataLesson <- readxl::read_excel(file_lesson)

  out$dataFact <- readxl::read_excel(file_fact)

  out$dataResponse <- readxl::read_excel(file_response)

  jsonToDataFrame <- function(string) {
    # parsedJSON <- jsonlite::fromJSON(string)
    parsedJSON <- jsonlite::fromJSON(txt=gsub("null", "\"\"", string))
    # parsedJSON <- jsonlite::fromJSON(txt=gsub("null", "NA", string))
    # parsedJSON <- do.call(c, jsonlite:::null_to_na(jsonlite::fromJSON(string)))
    # parsedJSON <- jsonlite::fromJSON(txt=gsub("\"\"", "\"", string))
    parsedJSON[[14]] = NULL
    parsedJSON[[12]] = NULL
    parsedJSON[[8]] = NULL
    return(parsedJSON)
  }

  colBindJson <- function(dataFrame) {
    colnames(dataFrame) <- c("userId", "factId_gen", "lessonId", "sequence_number",
                             "data", "create_time")

    newList <- jsonToDataFrame(dataFrame$data[1])

    # Create empty data from for json data
    jsonData <- data.frame(matrix(NA, nrow = nrow(dataFrame), ncol = length(newList)))

    # Set column headers from parsed json
    colnames(jsonData) <- names(newList)

    #Set column types for new dataframe

    # read JSON and convert to data.frame row
    for (rowNumber in 1:nrow(dataFrame)) {
      cat("rownumber", rowNumber, "\n")
      dataList <- jsonToDataFrame(dataFrame$data[rowNumber])
      # dataDf <- as.data.frame(matrix(unlist(dataList),nrow=length(dataList),byrow=TRUE))
      if(dataList$reactionTime == "") {dataList$reactionTime <- NA}
      dataDf <- as.data.frame(do.call(rbind, dataList))
      # Transpose the list (which is interpreted as a column), to a row
      jsonData[rowNumber,] <- t(dataDf[1])
    }
    as.data.frame(lapply(jsonData, type.convert))

    combinedDF <- cbind(dataFrame, jsonData)

    return(combinedDF)
  }

  out$ResponseFinal <- colBindJson(out$dataResponse)







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
