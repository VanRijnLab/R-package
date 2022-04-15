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
read_dataset_excel <- function(lessons, file_response = "../SlimStampen_data_examples/vocatrainer/medium_response.xlsx",
                               file_lesson = "../SlimStampen_data_examples/vocatrainer/nmd_live_vocatrainer_public_lesson.xlsx",
                               file_fact = "../SlimStampen_data_examples/vocatrainer/nmd_live_vocatrainer_public_fact.xlsx") {
  # if(file == system.file("extdata", "files.xlsx", package = "SlimStampeRData")) {
  #   message("Example dataset was used: excel sample")
  # }
  if(missing(lessons)){
    stop("No lessons are provided. Please provide lessonId's in a vector format: c(1, 2)")
  }
  cat("For big files this may take a few minutes. \n Rows with missing data are removed. \n")
  # start_time <- format(Sys.time(), "%Hh%Mm%Ss")
  # cat("Start time: ", start_time, "\n")


  out <- list()

  out$dataLesson <- readxl::read_excel(file_lesson, guess_max = 10000)

  out$dataFact <- readxl::read_excel(file_fact, guess_max = 10000)

  out$dataResponse <- readxl::read_excel(file_response, guess_max = 10000)

  renamedResponse = dplyr::rename(out$dataResponse, "userId" = "user_id", "factId_gen" = "fact_id", "lessonId" = "lesson_id")

  # Filter dataResponse on lessonId from input
  filterResponse <- filterdata(renamedResponse, lessons)

  # Parse JSON
  out$parsedResponse <- colBindJson(filterResponse)

  #Gathering data
  lessonResponse <- findlesson(out$parsedResponse, out$dataLesson)
  out$finalResponse <- findfact(lessonResponse, out$dataFact)








  cols <- c("factId", "userId", "sessionTime", "reactionTime", "correct", "lessonTitle", "lessonId","sessionId",
            "factText")
  missingcol <- missing_columns_check(out$finalResponse, cols)

  if(length(missingcol) > 0){
    strCols <- paste(cols,collapse=" ")
    warning("One or more of these columns is missing: ", strCols,"\n Some of the funcions in this package may not work if these columns are not provided")
  }

  # # Separate messages for alpha and repetition
  # colsalpha <- c("alpha")
  # missingalpha <- missing_columns_check(out$finalResponse, colsalpha)
  # if(length(missingalpha) > 0){
  #   cat("! No alpha column is provided in the data, run calculate_alpha_and_activation() to add an alpha column to the data \n")
  # }
  #
  # colsrep <- c("repetition")
  # missingrep <- missing_columns_check(out$finalResponse, colsrep)
  # if(length(missingrep) > 0){
  #   cat("! No repetition column is provided in the data, run calculate_repetition() to add a repetition column to the data \n")
  # }

  # end_time <- format(Sys.time(), "%Hh%Mm%Ss")
  # cat("End time: ", end_time, "\n")

  cat("Done! \n")

  return(out$finalResponse)
}

jsonToDataFrame <- function(string) {
  parsedJSON <- rjson::fromJSON(json_str =gsub("null", "\"\"", string))
  # parsedJSON <- do.call(c, jsonlite:::null_to_na(jsonlite::fromJSON(string)))
  parsedJSON[[14]] = NULL
  parsedJSON[[12]] = NULL
  parsedJSON[[8]] = NULL
  return(parsedJSON)
}

colBindJson <- function(dataFrame) {
  newList <- jsonToDataFrame(dataFrame$data[1])

  # Create empty data from for json data
  jsonData <- data.frame(matrix(NA, nrow = nrow(dataFrame), ncol = length(newList)))

  # Set column headers from parsed json
  colnames(jsonData) <- names(newList)

  counter = 0;

  datarows <- nrow(dataFrame)
  cat("Processing ", datarows, " rows. \n")
  cat("Progress: \n 0 %...")
  # read JSON and convert to data.frame row
  for (rowNumber in 1:datarows) {
    if(datarows < 1000){
      if(rowNumber%%100 == 0) {cat(round((rowNumber/datarows)*100),"%...")}
    } else {
      if(rowNumber%%1000 == 0) {cat(round((rowNumber/datarows)*100),"%...")}
    }
    # cat("rownumber", rowNumber, "\n")

    dataList <- jsonToDataFrame(dataFrame$data[rowNumber])

    if(length(dataList) == length(newList)) {
      if(dataList$reactionTime == "") {dataList$reactionTime <- NA}
      dataDf <- as.data.frame(matrix(unlist(dataList),nrow=length(dataList),byrow=TRUE))
      # Transpose the list (which is interpreted as a column), to a row
      jsonData[rowNumber,] <- t(dataDf[1])
    } else {
      counter = counter + 1
    }
  }
  jsonData <- as.data.frame(lapply(jsonData, type.convert, as.is = TRUE))

  combinedDF <- cbind(dataFrame, jsonData)

  if(counter > 0){
    cat("\n", counter, "row(s) were removed. \n")
  }

  return(combinedDF)
}

findlesson <- function(dataframe, lessonInfo) {
  dataframe$lessonTitle <- lessonInfo$title[match(dataframe$lessonId, lessonInfo$id)]

  return(dataframe)
}

findfact <- function(dataframe, factInfo) {
  dataframe$factText <- factInfo$text[match(dataframe$factId, factInfo$id)]
  dataframe$factAnswer <- factInfo$answer[match(dataframe$factId, factInfo$id)]

  return(dataframe)
}

filterdata <- function(data, lessons) {
  data <- dplyr::filter(data, lessonId %in% lessons)

  return(data)
}


