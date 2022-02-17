#' Plot average rate of forgetting over repetitions
#'
#' Plots the average rate of forgetting of every session over the repetitions of the facts.
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
average_ROF_over_repetition <- function(data, xlim = NULL, ylim = NULL, filepath = "../Figures") {
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

  participants <- unique(data$sessionId)

  plot <- NULL



  cat("This may take a moment... \n")
  plotTitle <- paste("ROF over repetition")
  # group by sessionId and repetition, then mean alpha as new column
  dat1 <- dplyr::group_by(.data = data, sessionId, repetition)
  dat2 <- dplyr::summarise(.data = dat1, mean_alpha = mean(alpha, na.rm=TRUE))

  maxTime <- max(data$repetition)
  upperRT <- max(dat2$mean_alpha)
  lowerRT <- min(dat2$mean_alpha)

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

  # Make plot
  plot <- ggplot2::ggplot(data = dat2, ggplot2::aes(x = factor(repetition), y = mean_alpha, group = sessionId)) +
    ggplot2::geom_line(alpha = 1, ggplot2::aes(colour = factor(sessionId))) +
    ggplot2::geom_point(alpha = 0.5, size = 1, ggplot2::aes(colour = factor(sessionId), fill = factor(sessionId))) +
    ggplot2::guides(colour = "none", fill = "none") +
    ggplot2::scale_color_viridis_d() +
    ggplot2::coord_cartesian(xlim = x, ylim = y) +
    ggplot2::labs(x = "Fact Repetitions", y = "Alpha") +
    ggplot2::ggtitle(plotTitle)

  # Save all plots to a pdf file
  date <- format(Sys.time(), "%d-%b-%Y %Hh%Mm")
  title <- paste("Average_ROF_over_repetition_", date, ".pdf")
  ggplot2::ggsave(title, plot, device = "pdf", path = filepath, width = 25, height = 20, units = "cm")

  cat("PDF of plot can be found in: ", filepath)

  # Display plot in viewer
  return(plot)
}
