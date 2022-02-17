#' Plot average accuracy over repetitions
#'
#' Plots the average accuracy of every session over the repetitions of the facts.
#'
#' Assumes that the dataset has a repetition column with fact repetition in
#' integers
#'
#' @param data A data frame. NA values will be removed before plotting.
#' @param xlim A vector of 2 (for example: c(0, 10)), indicating the range of
#'   the x-axis.If NULL the default value is used: c(0, max(repetition)).
#' @param ylim A vector of 2 (for example: c(0, 1000)), indicating the range of
#'   the y-axis.If NULL the default value is used: c(min(RT), mean(RT) +
#'   (SD(RT))).
#' @param filepath A relative or explicit path where plots will be saved
#' @return data frame
#' @export
average_accuracy_over_repetition <- function(data, xlim = NULL, ylim = NULL, filepath = "../Figures") {
  if(missing(data)){
    stop("No data is provided")
  }
  if(!("repetition" %in% colnames(data))){
    stop("No repetition column is provided in the data, run calculate_repetition()
         to add a repetition column to the data")
  }
  if(!(is.null(xlim) | length(xlim) == 2)){
    stop("xlim must be a vector of 2")
  }
  if(!(is.null(ylim) | length(ylim) == 2)){
    stop("ylim must be a vector of 2")
  }
  indx <- apply(data, 2, function(x) any(is.na(x)))
  if("repetition" %in% colnames(data)[indx]){
    cat("! There are missing values in the column repetition. \n")
  }
  if("sessionId" %in% colnames(data)[indx]){
    cat("! There are missing values in the column sessionId. \n")
  }
  if("correct" %in% colnames(data)[indx]){
    cat("! There are missing values in the column correct. \n")
  }

  participants <- unique(data$sessionId)

  plot <- NULL

  maxTime <- max(data$repetition)
  meanRT <- mean(data$correct, na.rm=TRUE)
  sdRT <- stats::sd(data$correct, na.rm=TRUE)
  upperRT <- meanRT + (sdRT)
  lowerRT <- 0

  if(is.null(xlim)){
    x = c(1, maxTime)
  } else {
    x = xlim
  }
  if(is.null(ylim)){
    y = c(lowerRT, upperRT)
  } else {
    y = ylim
  }

  cat("This may take a moment... \n")
  plotTitle <- paste("Accuracy over repetition")
  # group by sessionId and repetition, then mean accuracy as new column
  dat1 <- dplyr::group_by(.data = data, sessionId, repetition)
  dat2 <- dplyr::summarise(.data = dat1, mean_accuracy = mean(correct, na.rm=TRUE))

  # Make plot
  plot <- ggplot2::ggplot(data = dat2, ggplot2::aes(x = factor(repetition), y = mean_accuracy, group = sessionId)) +
    ggplot2::geom_line(alpha = 1, ggplot2::aes(colour = factor(sessionId))) +
    ggplot2::geom_point(alpha = 0.5, size = 1, ggplot2::aes(colour = factor(sessionId), fill = factor(sessionId))) +
    ggplot2::guides(colour = "none", fill = "none") +
    ggplot2::scale_color_viridis_d() +
    ggplot2::coord_cartesian(xlim = x, ylim = y) +
    ggplot2::labs(x = "Fact Repetitions", y = "Accuracy") +
    ggplot2::ggtitle(plotTitle)

  # Save all plots to a pdf file
  date <- format(Sys.time(), "%d-%b-%Y %Hh%Mm")
  title <- paste("Average_accuracy_over_repetition_", date, ".pdf")
  ggplot2::ggsave(title, plot, device = "pdf", path = filepath, width = 25, height = 20, units = "cm")

  cat("PDF of plot can be found in: ", filepath)

  # Display plot in viewer
  return(plot)
}
