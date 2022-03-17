#' Plot average RT over repetitions
#'
#' Plots the average RT of every session over the repetitions of the facts.
#'
#' Assumes that the dataset has a repetition column with fact repetition in
#' integers
#'
#' @param data A data frame. NA values will be removed before plotting.
#' @param xlim A vector of 2 (for example: c(0, 10)), indicating the range of
#'   the x-axis.If NULL the default value is used.
#' @param ylim A vector of 2 (for example: c(0, 1000)), indicating the range of
#'   the y-axis.If NULL the default value is used: c(min(RT), mean(RT) +
#'   (SD(RT))).
#' @param filepath A relative or explicit path where plots will be saved
#' @return data frame
#' @export
average_RT_participants <- function(data, xlim = NULL, ylim = NULL, filepath = "../Figures") {
  if(missing(data)){
    stop("No data is provided")
  }
  if(!("repetition" %in% colnames(data))){
    stop("No repetition column is provided in the data, run calculate_repetition() to add a repetition column to the data")
  }
  missingcol <- missing_columns_check(data, c("sessionId", "reactionTime", "repetition"))
  if(length(missingcol) > 0){
    stop("No ", missingcol[[1]] ," column is provided in the data")
  }

  if(!(is.null(xlim) | length(xlim) == 2)){
    stop("xlim must be a vector of 2")
  }
  if(!(is.null(ylim) | length(ylim) == 2)){
    stop("ylim must be a vector of 2")
  }

  missing_values_message(data, c("sessionId", "reactionTime", "repetition"))

  participants <- sort(unique(data$sessionId))
  partcolor <- viridis::turbo(length(participants))
  names(partcolor)  <- participants

  cat("This may take a moment... \n")
  plotTitle <- paste("Average RT for every participant over repetition")
  plot <- NULL

  # Group by sessionId and repetition, then mean reaction time as new column
  dat1 <- dplyr::group_by(.data = data, sessionId, repetition)
  dat2 <- dplyr::summarise(.data = dat1, mean_RT = mean(reactionTime, na.rm=TRUE))

  x = xlim
  y = set_y(data$reactionTime, ylim)

  # Make plot
  plot <- ggplot2::ggplot(data = dat2, ggplot2::aes(x = factor(repetition), y = mean_RT, group = sessionId)) +
    ggplot2::geom_line(alpha = 1, ggplot2::aes(colour = factor(sessionId))) +
    ggplot2::geom_point(alpha = 0.5, size = 1, ggplot2::aes(colour = factor(sessionId), fill = factor(sessionId))) +
    ggplot2::guides(colour = "none", fill = "none") +
    ggplot2::scale_color_manual(values = partcolor) +
    ggplot2::coord_cartesian(xlim = x, ylim = y) +
    ggplot2::labs(x = "Fact Repetitions", y = "Reaction Time (ms)") +
    ggplot2::ggtitle(plotTitle)

  # Save all plots to a pdf file
  title <- paste("Average_RT_over_repetition_", title_time(), ".pdf")
  ggplot2::ggsave(title, plot, device = "pdf", path = filepath, width = 25, height = 20, units = "cm")

  cat("PDF of plot can be found in: ", filepath, "\n")

  # Display plot in viewer
  return(plot)
}


#' Plot average rate of forgetting over repetitions
#'
#' Plots the average rate of forgetting of every session over the repetitions of the facts.
#'
#' Assumes that the dataset has a repetition column with fact repetition in
#' integers and a column with the alpha's for all observations
#'
#' @param data A data frame. NA values will be removed before plotting.
#' @param xlim A vector of 2 (for example: c(0, 10)), indicating the range of
#'   the x-axis.If NULL the default value is used.
#' @param ylim A vector of 2 (for example: c(0, 1000)), indicating the range of
#'   the y-axis.If NULL the default value is used.
#' @param filepath A relative or explicit path where plots will be saved
#' @return data frame
#' @export
average_ROF_participants <- function(data, xlim = NULL, ylim = NULL, filepath = "../Figures") {
  if(missing(data)){
    stop("No data is provided")
  }
  if(!("repetition" %in% colnames(data))){
    stop("No repetition column is provided in the data, run calculate_repetition() to add a repetition column to the data")
  }
  if(!("alpha" %in% colnames(data))){
    stop("No alpha column is provided in the data, run calculate_alpha_and_activation() to add an alpha column to the data")
  }
  missingcol <- missing_columns_check(data, c("sessionId", "alpha", "repetition"))
  if(length(missingcol) > 0){
    stop("No ", missingcol[[1]] ," column is provided in the data")
  }

  if(!(is.null(xlim) | length(xlim) == 2)){
    stop("xlim must be a vector of 2")
  }
  if(!(is.null(ylim) | length(ylim) == 2)){
    stop("ylim must be a vector of 2")
  }

  missing_values_message(data, c("sessionId", "alpha", "repetition"))

  participants <- sort(unique(data$sessionId))
  partcolor <- viridis::turbo(length(participants))
  names(partcolor)  <- participants

  cat("This may take a moment... \n")
  plotTitle <- paste("Average ROF for every participant over repetition")
  plot <- NULL

  # Group by sessionId and repetition, then mean alpha as new column
  dat1 <- dplyr::group_by(.data = data, sessionId, repetition)
  dat2 <- dplyr::summarise(.data = dat1, mean_alpha = mean(alpha, na.rm=TRUE))

  y = ylim
  x = xlim

  # Make plot
  plot <- ggplot2::ggplot(data = dat2, ggplot2::aes(x = factor(repetition), y = mean_alpha, group = sessionId)) +
    ggplot2::geom_line(alpha = 1, ggplot2::aes(colour = factor(sessionId))) +
    ggplot2::geom_point(alpha = 0.5, size = 1, ggplot2::aes(colour = factor(sessionId), fill = factor(sessionId))) +
    ggplot2::guides(colour = "none", fill = "none") +
    ggplot2::scale_color_manual(values = partcolor) +
    ggplot2::coord_cartesian(xlim = x, ylim = y) +
    ggplot2::labs(x = "Fact Repetitions", y = "Alpha") +
    ggplot2::ggtitle(plotTitle)

  # Save all plots to a pdf file
  title <- paste("Average_ROF_over_repetition_", title_time(), ".pdf")
  ggplot2::ggsave(title, plot, device = "pdf", path = filepath, width = 25, height = 20, units = "cm")

  cat("PDF of plot can be found in: ", filepath, "\n")

  # Display plot in viewer
  return(plot)
}

#' Plot average accuracy over repetitions
#'
#' Plots the average accuracy of every session over the repetitions of the facts.
#'
#' Assumes that the dataset has a repetition column with fact repetition in
#' integers
#'
#' @param data A data frame. NA values will be removed before plotting.
#' @param xlim A vector of 2 (for example: c(0, 10)), indicating the range of
#'   the x-axis.If NULL the default value is used.
#' @param ylim A vector of 2 (for example: c(0, 1000)), indicating the range of
#'   the y-axis.If NULL the default value is used: c(0, 1).
#' @param filepath A relative or explicit path where plots will be saved
#' @return data frame
#' @export
average_accuracy_participants <- function(data, xlim = NULL, ylim = NULL, filepath = "../Figures") {
  if(missing(data)){
    stop("No data is provided")
  }
  if(!("repetition" %in% colnames(data))){
    stop("No repetition column is provided in the data, run calculate_repetition() to add a repetition column to the data")
  }
  if(!(is.null(xlim) | length(xlim) == 2)){
    stop("xlim must be a vector of 2")
  }
  if(!(is.null(ylim) | length(ylim) == 2)){
    stop("ylim must be a vector of 2")
  }
  missingcol <- missing_columns_check(data, c("sessionId", "correct", "repetition"))
  if(length(missingcol) > 0){
    stop("No ", missingcol[[1]] ," column is provided in the data")
  }

  missing_values_message(data, c("sessionId", "correct", "repetition"))

  participants <- sort(unique(data$sessionId))
  partcolor <- viridis::turbo(length(participants))
  names(partcolor)  <- participants


  x = xlim
  if(is.null(ylim)){
    y = c(0, 1)
  } else {
    y = ylim
  }

  cat("This may take a moment... \n")
  plotTitle <- paste("Average Accuracy for every participant over repetition")
  plot <- NULL

  # Group by sessionId and repetition, then mean accuracy as new column
  dat1 <- dplyr::group_by(.data = data, sessionId, repetition)
  dat2 <- dplyr::summarise(.data = dat1, mean_accuracy = mean(correct, na.rm=TRUE))

  # Make plot
  plot <- ggplot2::ggplot(data = dat2, ggplot2::aes(x = factor(repetition), y = mean_accuracy, group = sessionId)) +
    ggplot2::geom_line(alpha = 1, ggplot2::aes(colour = factor(sessionId))) +
    ggplot2::geom_point(alpha = 0.5, size = 1, ggplot2::aes(colour = factor(sessionId), fill = factor(sessionId))) +
    ggplot2::guides(colour = "none", fill = "none") +
    ggplot2::scale_color_manual(values = partcolor) +
    ggplot2::coord_cartesian(xlim = x, ylim = y) +
    ggplot2::labs(x = "Fact Repetitions", y = "Accuracy") +
    ggplot2::ggtitle(plotTitle)

  # Save all plots to a pdf file
  title <- paste("Average_accuracy_over_repetition_", title_time(), ".pdf")
  ggplot2::ggsave(title, plot, device = "pdf", path = filepath, width = 25, height = 20, units = "cm")

  cat("PDF of plot can be found in: ", filepath, "\n")

  # Display plot in viewer
  return(plot)
}

#' Plot average rate of forgetting over repetitions for each fact
#'
#' Plots the average rate of forgetting for each fact over the repetitions of
#' the facts.
#'
#' Assumes that the dataset has a repetition column with fact repetition in
#' integers and a column with the alpha's for all observations.
#'
#' The legend is given in the order of the average alpha for the last repetition
#' of each fact. If there is more than one column (if there are too many facts
#' to fit in one column), the columns should be read one-by-one. The upper-left
#' value is the highest last alpha, the lower- right value is the lowest last
#' alpha. All last alpha's are also marker by a bigger marker in the plot. Be
#' aware that lines/markers may sometimes overlap.
#'
#' @param data A data frame. NA values will be removed before plotting.
#' @param factNames Column that shows the names of the facts (like factText or factAnswer). Default shows factId.
#' @param xlim A vector of 2 (for example: c(0, 10)), indicating the range of
#'   the x-axis.If NULL the default value is used.
#' @param ylim A vector of 2 (for example: c(0, 1000)), indicating the range of
#'   the y-axis.If NULL the default value is used.
#' @param filepath A relative or explicit path where plots will be saved
#' @return data frame
#' @export
average_ROF_facts <- function(data, factNames = "factId", xlim = NULL, ylim = NULL, filepath = "../Figures") {
  if(missing(data)){
    stop("No data is provided")
  }
  if(!("repetition" %in% colnames(data))){
    stop("No repetition column is provided in the data, run calculate_repetition() to add a repetition column to the data")
  }
  if(!("alpha" %in% colnames(data))){
    stop("No alpha column is provided in the data, run calculate_alpha_and_activation() to add an alpha column to the data")
  }

  if(!(is.null(xlim) | length(xlim) == 2)){
    stop("xlim must be a vector of 2")
  }
  if(!(is.null(ylim) | length(ylim) == 2)){
    stop("ylim must be a vector of 2")
  }
  missingcol <- missing_columns_check(data, c("sessionId", "alpha", "repetition", "factId", factNames))
  if(length(missingcol) > 0){
    stop("No ", missingcol[[1]] ," column is provided in the data")
  }

  missing_values_message(data, c("sessionId", "alpha", "repetition", "factId", factNames))

  cat("This may take a moment... \n")
  plotTitle <- paste("Average ROF for every fact over repetition")
  plot <- NULL

  facts <- sort(unique(data$factId))
  factcolor <- viridis::turbo(length(facts))
  names(factcolor)  <- facts

  # Group by sessionId and repetition, then mean alpha as new column
  dat1 <- dplyr::group_by(.data = data, factId, repetition)
  dat2 <- dplyr::summarise(.data = dat1, mean_alpha = mean(alpha, na.rm=TRUE), factLabels = unique(.data[[factNames]])[[1]])
  dat2$factLabels = substr(dat2$factLabels, 1, 10)
  dat3 <- dplyr::group_by(.data = dat2, factId)
  dat4 <- dplyr::filter(.data = dat3, repetition == max(repetition))
  dat5 <- dat4[order(dat4$mean_alpha, decreasing = TRUE),]

  y = ylim
  x = xlim

  # Make plot
  plot <- ggplot2::ggplot(data = dat2, ggplot2::aes(x = factor(repetition), y = mean_alpha, group = factId)) +
    ggplot2::geom_line(alpha = 1, ggplot2::aes(colour = factor(factId))) +
    ggplot2::geom_point(alpha = 0.5, size = 1, ggplot2::aes(colour = factor(factId), fill = factor(factId))) +
    ggplot2::geom_point(data = dat5, alpha = 0.5, size = 3, position = ggplot2::position_jitter(w = 0.08, h = 0), ggplot2::aes(colour = factor(factId), fill = factor(factId))) +
    ggplot2::scale_color_manual(name="Facts", labels = dat5$factLabels,values = factcolor, breaks=dat5$factId) +
    ggplot2::guides(fill = "none") +
    ggplot2::coord_cartesian(xlim = x, ylim = y) +
    ggplot2::labs(x = "Fact Repetitions", y = "Alpha") +
    ggplot2::ggtitle(plotTitle)

  # Save all plots to a pdf file
  title <- paste("av_ROF_repetition_fact_", title_time(), ".pdf")
  ggplot2::ggsave(title, plot, device = "pdf", path = filepath, width = 25, height = 20, units = "cm")

  cat("PDF of plot can be found in: ", filepath, "\n")

  # Display plot in viewer
  return(plot)
}

