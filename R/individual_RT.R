#' Plot RT for all Facts for all Participants over Time
#'
#' \code{individual_RT} plots the RT of all sessions over time. Incorrect
#' answers are denoted with a red marker.
#'
#' @family individual functions
#'
#' @param data A data frame. NA values will be removed before plotting.
#' @param session Provide a single sessionId string to plot that session. If
#'   sessionId is NULL all sessions will be plotted.
#' @param normalizeTime If TRUE, the times of all facts will be normalized (they
#'   will start at 0). If FALSE, the times will not be normalized and data
#'   points will occur relative to their occurrence during the session.
#' @param xlim A vector of 2 (for example: c(0, 10)), indicating the range of
#'   the x-axis.If NULL the default value is used: c(0, z). Where z is the max
#'   time.
#' @param ylim A vector of 2 (for example: c(0, 10)), indicating the range of
#'   the y-axis.If NULL the default value is used: c(a, b). Where a is the
#'   minimum reaction time and b is the average reaction time plus the standard
#'   deviation of the reaction time.
#' @param filepath A relative or explicit path where plots will be saved
#' @return A preview plot in the viewer and a pdf file in filepath
#' @export
individual_RT <- function(data, session = NULL, normalizeTime = FALSE, logarithmic = FALSE, xlim = NULL, ylim = NULL, filepath = NULL) {
  if(missing(data)){
    stop("No data is provided")
  }
  if(!(is.null(xlim) | length(xlim) == 2)){
    stop("xlim must be a vector of 2")
  }
  if(!(is.null(ylim) | length(ylim) == 2)){
    stop("ylim must be a vector of 2")
  }

  missingcol <- missing_columns_check(data, c("sessionId", "factId", "reactionTime", "correct", "lessonId", "userId", "presentationStartTime", "lessonTitle"))
  if(length(missingcol) > 0){
    stop("No ", missingcol[[1]] ," column is provided in the data")
  }

  if(-1 %in% data$factId){
    data <- resetremoval(data)
    cat("- There are resets present in the data. Reset data is excluded in this function. - \n")
  }

  missing_values_message(data, c("sessionId", "factId", "reactionTime", "correct"))

  # Assign colors to facts
  facts <- sort(unique(data$factId))
  factcolor <- viridis::turbo(length(facts))
  names(factcolor)  <- facts

  # Single Session Settings
  sessionflag <- FALSE

  if(!is.null(session)){
    if(!(is.character(session) & length(session) == 1)){
      stop("Session is not a string")
    }
    sessionflag <- TRUE
    data <- dplyr::filter(data, sessionId == session)

  }

  # init plot lists
  plots <- list()
  plots4 <- list()

  # Determine axis
  sessiongroup <- dplyr::group_by(data, userId, sessionId)
  sessiontimes <- dplyr::summarise(sessiongroup, times = (max(presentationStartTime, na.rm = TRUE) - min(presentationStartTime, na.rm = TRUE))/60000)
  maxTime <- max(sessiontimes$times, na.rm = TRUE)
  if(is.null(xlim)){
    x = c(0, maxTime)
  } else {
    x = xlim
  }
  y <- set_y(data$reactionTime, ylim)

  cat("This may take a moment... \n")

  # Prepare data for graphs
  data <- dplyr::filter(data, !is.na(reactionTime))
  data$lessonTitle <- substr(data$lessonTitle, 1, 19)

  datagroup <- dplyr::group_by(data, lessonId, userId)
  datasort <- dplyr::arrange(datagroup, presentationStartTime)
  timediff <- dplyr::mutate(datasort, breakTime = presentationStartTime - dplyr::lag(presentationStartTime))
  sessiondiff <- dplyr::mutate(timediff, sessionOrder = cumsum(sessionId != dplyr::lag(sessionId, def = "none")))

  full_data <- NULL
    if(normalizeTime){
      tinygroup <- dplyr::group_by(sessiondiff, lessonId, userId, sessionId, factId)
      full_data <- dplyr::mutate(tinygroup, time = (presentationStartTime - min(presentationStartTime, na.rm = TRUE)) / 60000)
    } else {
      tinygroup <- dplyr::group_by(sessiondiff, lessonId, userId, sessionId)
      full_data <- dplyr::mutate(tinygroup, time = (presentationStartTime - min(presentationStartTime, na.rm = TRUE)) / 60000)
    }

  full_data <- dplyr::ungroup(full_data)

  split_data <- split(full_data, list(full_data$lessonId, full_data$userId, full_data$sessionId), drop = TRUE)

  # Plot graphs
  data_plots <- NULL
  if(logarithmic){
    data_plots <- purrr::map(split_data, ~ ggplot2::ggplot(data = ., ggplot2::aes(x = time, y = reactionTime)) +
                                              ggplot2::geom_line(alpha = 1, ggplot2::aes(colour = factor(factId))) +
                                              ggplot2::geom_point(alpha = 1, size = 1.5, stroke = 0, pch = 21, ggplot2::aes(fill = correct)) +
                                              ggplot2::guides(colour = "none", fill = "none") +
                                              ggplot2::scale_fill_manual(values = c("TRUE"="grey", "FALSE"= "red", "1"="grey", "0"= "red")) +
                                              ggplot2::scale_color_manual(values = factcolor) +
                                              ggplot2::coord_cartesian(xlim = x, ylim = y) +
                                              ggplot2::labs(x = "Time (minutes)", y = "Reaction Time (ms)") +
                                              ggplot2::scale_y_log10(breaks = scales::log_breaks(n = 6, base = 10)) +
                                              ggplot2::ggtitle( label = paste("Lesson: ", .x$lessonTitle[1], ",User: ", .x$userId[1]),
                                                                subtitle = paste("Session #", .x$sessionOrder[1], ",Since last session:", ms_to_string(.x$breakTime[1]) )))


  } else {
    data_plots <- purrr::map(split_data, ~ ggplot2::ggplot(data = ., ggplot2::aes(x = time, y = reactionTime)) +
                                              ggplot2::geom_line(alpha = 1, ggplot2::aes(colour = factor(factId))) +
                                              ggplot2::geom_point(alpha = 1, size = 1.5, stroke = 0, pch = 21, ggplot2::aes(fill = correct)) +
                                              ggplot2::guides(colour = "none", fill = "none") +
                                              ggplot2::scale_fill_manual(values = c("TRUE"="grey", "FALSE"= "red", "1"="grey", "0"= "red")) +
                                              ggplot2::scale_color_manual(values = factcolor) +
                                              ggplot2::coord_cartesian(xlim = x, ylim = y) +
                                              ggplot2::labs(x = "Time (minutes)", y = "Reaction Time (ms)") +
                                              ggplot2::ggtitle( label = paste("Lesson: ", .x$lessonTitle[1], ",User: ", .x$userId[1]),
                                                                subtitle = paste("Session #", .x$sessionOrder[1], ",Since last session:", ms_to_string(.x$breakTime[1]) )))

  }
  plots <- as.list(data_plots)
  plots <- plots[order(sapply(plots, function(x) x$data$userId[1]), sapply(plots, function(x) x$data$lessonId[1]), sapply(plots, function(x) x$data$sessionOrder[1]))]
  plots4 <- plots[1:4]

  # Print plots
  res <- NULL
  title <- paste("Individual_RT_", title_time(), ".pdf")
  fileplace <- filepath
  if(is.null(filepath)){
    fileplace <- getwd()
  }
  if(sessionflag){
    res <- plots[[1]]

    # Save plot to a pdf file
    ggplot2::ggsave(title, res, device = "pdf", path = filepath, width = 25, height = 20, units = "cm")

    cat("PDF of plot can be found in: ", fileplace, "\n")

  } else {
    res <- cowplot::plot_grid(plotlist = plots4, nrow = 2, ncol = 2)

    # Save all plots to a pdf file
    ggplot2::ggsave(title, gridExtra::marrangeGrob(grobs = plots, nrow=2, ncol=2, layout_matrix = matrix(1:4, 2, 2, TRUE)),
                    device = "pdf", path = filepath, width = 22, height = 22, units = "cm")

    cat("Preview of the first 4 plots are displayed in viewer. \n")
    cat("PDF of plots can be found in: ", fileplace, "\n")
  }

  return(res)

}
