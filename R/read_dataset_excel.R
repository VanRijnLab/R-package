#' Read data set from excel files
#'
#' \code{read_dataset_excel} reads the raw data set from multiple excel files
#' into a data frame with the correct settings.
#'
#' Three files need to be provided to parse the excel files correctly. These are
#' the files containing the responses, the lesson data and the fact data. The
#' file path to these files can either be provided individually or a directory
#' containing these files can be provided. If a directory is provided the files
#' must end on "fact.xlsx", "lesson.xlsx" and "response.xlsx" to be recognized.
#' Only one file ending on each of these strings should be present in the
#' directory, otherwise the wrong files may be selected.
#'
#' If one of your files is placed in a different directory or does not end on
#' the correct string, it is necessary to provide all three files with their
#' individual file path and to not provide a directory.
#'
#' @family import data functions
#'
#' @param lessons A vector that contains the lesson ID's that should be parsed.
#' @param file_response Excel file with response entries.
#' @param file_lesson Excel file with lesson data.
#' @param file_fact Excel file with fact data.
#' @param file_dir Directory that contains the excel files ending on
#'   "fact.xlsx", "lesson.xlsx" and "response.xlsx".
#' @param exampleset If TRUE the example data set will be used and no excel
#'   files need to be provided. The default is FALSE, and either a directory or
#'   a set of 3 files need to be provided.
#'
#' @return A data table. The output has the following properties:
#'
#' * The information contained in the data column from the response file is
#' separated into individual columns. Except for properties that contain
#' objects, which are not parsed.
#' * All other data from the response file is
#' preserved. From the lesson file the lesson title is added to the response
#' entries. From the fact file the fact text and fact answer is added to the
#' response entries.
#' * A presentationStartTime is added to reset entries (with
#' factId -1), which do not represent the time at which the reset was performed,
#' but do represent the sequence of the entries.
#' @export
#'
read_dataset_excel <- function(lessons = NULL, file_response = NULL,
                               file_lesson = NULL, file_fact = NULL,
                               file_dir = NULL, exampleset = FALSE) {

  cat("For big files this may take a few minutes. \n")

  if(exampleset == TRUE){
    lessons = c(38, 39)

    file_response = system.file("extdata", "example_response.xlsx", package = "SlimStampeRData")
    file_lesson = system.file("extdata", "example_lesson.xlsx", package = "SlimStampeRData")
    file_fact = system.file("extdata", "example_fact.xlsx", package = "SlimStampeRData")
    message("Example dataset was used: excel sample")
  }

  if(is.null(lessons)){
    stop("No lessons are provided. Please provide lessonId's in a vector format: c(1, 2)")
  }

  if(is.null(file_dir) && (is.null(file_response) || is.null(file_fact) || is.null(file_lesson))) {
    stop("Missing input files. Either provide a directory that contains the files or provide all three files as seperate input. (See documentation for more information)")

  }


  out <- list()
  if(!is.null(file_dir)){
    files <- readDirectory(file_dir)
    out$dataLesson <- readxl::read_excel(files$lesson, guess_max = 10000)

    out$dataFact <- readxl::read_excel(files$fact, guess_max = 10000)

    out$dataResponse <- readxl::read_excel(files$response, guess_max = 10000)
  } else {
    out$dataLesson <- readxl::read_excel(file_lesson, guess_max = 10000)

    out$dataFact <- readxl::read_excel(file_fact, guess_max = 10000)

    out$dataResponse <- readxl::read_excel(file_response, guess_max = 10000)
  }


  renamedResponse = dplyr::rename(out$dataResponse, "userId" = "user_id", "factId_gen" = "fact_id", "lessonId" = "lesson_id")

  # Filter dataResponse on lessonId from input
  filterResponse <- filterdata(renamedResponse, lessons)

  if(nrow(filterResponse) == 0) {
    stop("No trials for these lessons present in the file, please select other lessons.")
  }

  # Parse JSON
  out$parsedResponse <- colBindJson(filterResponse)

  #Gathering data
  lessonResponse <- findlesson(out$parsedResponse, out$dataLesson)
  out$combinedData <- findfact(lessonResponse, out$dataFact)

  out$finalResponse <- addResetTime(out$combinedData)




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

  cat("Done! \n")

  return(out$finalResponse)
  # return(out)
}

jsonToDataFrame <- function(string, newList) {
  parsedJSON <- rjson::fromJSON(json_str =gsub("null", "\"\"", string))
  # parsedJSON <- do.call(c, jsonlite:::null_to_na(jsonlite::fromJSON(string)))

  parsedJSON[["alternatives"]] = NULL
  parsedJSON[["keyStrokes"]] = NULL
  parsedJSON[["modelParameters"]] = NULL

  # find out what data is missing
  target <- names(parsedJSON)
  pop <- names(newList)
  missingName <- pop[!(pop%in%target)]
  extraName <- target[!(target%in%pop)]

  if(length(parsedJSON) != length(newList) || length(missingName) != 0){
    # cat("missing: ", missingName, "\n")
    # cat("extra: ", extraName, "\n")

    # remove extra data
    for (extra in extraName) {
      parsedJSON[[extra]] = NULL
    }
    # fill missing data with NA values
    for (name in missingName) {
      x <- match(name, names(newList))
      parsedJSON <- append(parsedJSON, NA, after = (x-1))
      names(parsedJSON)[x] <- name
    }
  }
  if(!is.na(parsedJSON[["reactionTime"]]) && parsedJSON[["reactionTime"]] == "") {parsedJSON[["reactionTime"]] <- NA}

  return(parsedJSON)
}

dataTemplate <- function(string) {
  parsedJSON <- rjson::fromJSON(json_str =gsub("null", "\"\"", string))
  parsedJSON[["alternatives"]] = NULL
  parsedJSON[["keyStrokes"]] = NULL
  parsedJSON[["modelParameters"]] = NULL
  if(length(parsedJSON) > 0 ){
    for (i in 1:length(parsedJSON)) {
      if(is.list(parsedJSON[[i]])){
        parsedJSON[[i]] <- NULL
      }
    }
  }


  return(parsedJSON)
}

colBindJson <- function(dataFrame) {
  maxRow <- 0
  rownum <- NULL
  temprow <- ifelse(nrow(dataFrame) > 10, 10, nrow(dataFrame))
  for (i in 1:temprow) {
    if (length(dataTemplate(dataFrame$data[i])) > maxRow){
      maxRow = length(dataTemplate(dataFrame$data[i]));
      rownum = i;
    }
  }
  if(is.null(rownum)){
    stop("No data detected within the first 10 rows.")
  }
  newList <- dataTemplate(dataFrame$data[rownum])
  datacols <- length(newList)

  # Create empty data from for json data
  jsonData <- data.frame(matrix(NA, nrow = nrow(dataFrame), ncol = length(newList)))

  # Set column headers from parsed json
  colnames(jsonData) <- names(newList)

  counter = 0;
  # wrongrows <- list()
  # wrongrows[[1]] <- newList

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

    dataList <- jsonToDataFrame(dataFrame$data[rowNumber], newList)

    if(length(dataList) == length(newList)) {
      dataDf <- as.data.frame(matrix(unlist(dataList),nrow=length(dataList),byrow=TRUE))
      # Transpose the list (which is interpreted as a column), to a row
      jsonData[rowNumber,] <- t(dataDf[1])
    } else {
      counter = counter + 1
      # cat("wrong row: ", rowNumber, "\n")
      # wrongrows[[counter+2]] <- dataList
    }
  }

  jsonData <- as.data.frame(lapply(jsonData, utils::type.convert, as.is = TRUE, numerals = "no.loss"))
  if("presentationStartTime" %in% names(jsonData)){jsonData$presentationStartTime <- bit64::as.integer64(jsonData$presentationStartTime)}


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

readDirectory <- function(file_dir) {
  files <- list()
  factfiles <- list.files(file_dir, pattern='fact\\.xlsx$')
  lessonfiles <- list.files(file_dir, pattern='lesson\\.xlsx$')
  responsefiles <- list.files(file_dir, pattern='response\\.xlsx$')

  if(length(factfiles) < 1) {
    stop("The fact file is not present in this directory. The file is expected to end with 'fact.xlsx'")
  }
  if( length(lessonfiles) < 1 ) {
    stop("The lesson file is not present in this directory. The file is expected to end with 'lesson.xlsx'")

  }
  if(length(responsefiles) < 1 ) {
    stop("The response file is not present in this directory. The file is expected to end with 'response.xlsx'")
  }

  factpath <- file.path(file_dir, factfiles[1])
  lessonpath <- file.path(file_dir, lessonfiles[1])
  responsepath <- file.path(file_dir, responsefiles[1])

  files$fact <- factpath
  files$lesson <- lessonpath
  files$response <- responsepath

  return(files)
}

addResetTime <- function(dataframe) {
  # functions relies on presentationStartTime and sequence_number (otherwise
  # skip and warning)
  participants <- unique(dataframe$userId)
  lessons <- unique(dataframe$lessonId)
  df <- data.frame(lessonId = 0, userId = 0, StartTime = bit64::as.integer64(0), sequence_number = 0, fact = -1)
  foundreset = 0;
  for (les in lessons) {
    for(par in participants){
      data1 <- dplyr::arrange(dplyr::filter(dataframe, lessonId == les & userId == par), sequence_number)
      while(any(is.na(data1$presentationStartTime))){
        foundreset = 1;
        index = which(is.na(data1$presentationStartTime))[1];
        if(index == 1){
          data1$presentationStartTime[index] <- 0
        } else {
          data1$presentationStartTime[index] <- data1$presentationStartTime[index-1]+1
        }
        df <- rbind(df, data.frame(lessonId = les, userId = par, StartTime = data1$presentationStartTime[index], sequence_number = data1$sequence_number[index], fact = -1))
      }
    }
  }
  # match dataframe to df
  joindata <- dplyr::left_join(dataframe, df, by = c("lessonId", "userId", "sequence_number"))
  mergetime <- dplyr::mutate(joindata, presentationStartTime= dplyr::coalesce(presentationStartTime, StartTime), factId= dplyr::coalesce(factId, fact))
  selectdata <- dplyr::select(mergetime, -StartTime, -fact)

  if(foundreset){
    cat("\n - Reset entries have been found, presentationStartTime is being estimated. - \n")
  }

  return(selectdata)
}
