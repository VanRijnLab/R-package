# Sets the x-axis limits
set_x <- function(column, xlim) {
  maxTime <- max(column)
  minTime <- min(column)
  if(is.null(xlim)){
    x = c(minTime, maxTime)
  } else {
    x = xlim
  }
  return(x)
}

# Sets the y-axis limits
set_y <- function(column, ylim) {
  meancol <- mean(column, na.rm=TRUE)
  sdcol <- stats::sd(column, na.rm=TRUE)
  uppercol <- meancol + sdcol
  lowercol <- min(column)
  if(is.null(ylim)){
    y = c(lowercol, uppercol)
  } else {
    y = ylim
  }
  return(y)
}

# Prints a message when the column contains missing values
missing_values_message <- function(data, columns) {
  indx <- apply(data, 2, function(x) any(is.na(x)))
  for(col in columns){
    if(col %in% colnames(data)[indx]){
      cat("! There are missing values in the column ", col, " ! \n")
    }
  }
}

# Returns a non-empty vector when a column is missing
missing_columns_check <- function(data, columns) {
  missingcols <- character(0)
  for(col in columns){
    if(!(col %in% colnames(data))){
      missingcols[[1]] <- col
    }
  }
  return(missingcols)
}

# Formats the current time for PDF file titles
title_time <- function() {
  return (format(Sys.time(), "%d-%b-%Y_%Hh%Mm%Ss"))
}

resetremoval <- function(df) {
  dfgroup <- dplyr::group_by(df, lessonId, userId)
  dfsort <- dplyr::arrange(dfgroup, presentationStartTime, .by_group = TRUE)
  dfslice <- dplyr::slice(dfsort, max(which(factId == -1), 1):n())
  dfclean <- dplyr::filter(dfslice, !factId==-1)

  return(dfclean)
}
