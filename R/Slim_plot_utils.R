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

missing_values_message <- function(data, columns) {
  indx <- apply(data, 2, function(x) any(is.na(x)))
  for(col in columns){
    if(col %in% colnames(data)[indx]){
      cat("! There are missing values in the column ", col, " ! \n")
    }
  }
}
