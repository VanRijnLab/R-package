# Sets the x-axis limits
set_x <- function(column, xlim) {
  maxTime <- max(column, na.rm = TRUE)
  minTime <- min(column, na.rm = TRUE)
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
  lowercol <- min(column, na.rm = TRUE)
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

# removes all data before the last user reset
resetremoval <- function(df) {
  dfgroup <- dplyr::group_by(df, lessonId, userId)
  dfsort <- dplyr::arrange(dfgroup, presentationStartTime, .by_group = TRUE)
  dfslice <- dplyr::slice(dfsort, max(which(factId == -1), 1):dplyr::n())
  dfclean <- dplyr::filter(dfslice, !factId==-1)
  dffree <- dplyr::ungroup(dfclean)

  return(dffree)
}

#Only the first session of a user within a lesson is kept in the data
firstsession <- function(df) {
  dfgroup <- dplyr::group_by(df, lessonId, userId)
  dfsort <- dplyr::arrange(dfgroup, presentationStartTime, .by_group = TRUE)
  dfclean <- dplyr::filter(dfsort, sessionId == unique(sessionId)[1])
  dffree <- dplyr::ungroup(dfclean)
  return(dffree)
}

#makes time in ms human-readable
ms_to_string <- function(time) {
  if(is.na(time)){
    return(paste("None"))
  } else if(time<1000){
    return(paste(time, "ms"))
  } else if (time <60000){
    return(paste(round(time/1000, digits=2), "sec"))
  } else if (time < 3600000){
    return(paste(round(time/60000, digits=2), "min"))
  } else if (time < 86400000){
    return(paste(round(time/3600000, digits=2), "hours"))
  } else if (time < 604800000){
    return(paste(round(time/86400000, digits=2), "days"))
  } else {
    return(paste(round(time/604800000, digits=2), "weeks"))
  }

}


