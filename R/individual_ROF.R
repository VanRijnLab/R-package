#' Plot ROF for all Facts for all Participants over Time
#'
#' \code{individual_ROF} plots the rate of forgetting (alpha) of all sessions
#' over time. Incorrect answers are denoted with a red marker.
#'
#' @family individual functions
#'
#' @param data A data frame. NA values will be removed before plotting.
#' @param sessionId Provide a single sessionId string to plot that session. If
#'   sessionId is NULL all sessions will be plotted.
#' @param normalizeTime If TRUE, the times of all facts will be normalized (they
#'   will start at 0). If FALSE, the times will not be normalized and data
#'   points will occur relative to their occurrence during the session.
#' @param xlim A vector of 2 (for example: c(0, 10)), indicating the range of
#'   the x-axis.If NULL the default value is used: c(0, z). Where z is the max
#'   time.
#' @param ylim A vector of 2 (for example: c(0, 10)), indicating the range of
#'   the y-axis.If NULL the default value is used: c(0.10, 0.5).
#' @param filepath A relative or explicit path where plots will be saved
#' @return A preview plot in the viewer and a pdf file in filepath
#' @export
individual_ROF <- function(data, sessionId = NULL, normalizeTime = FALSE, xlim = NULL, ylim = NULL, filepath = "../Figures") {
  if(missing(data)){
    stop("No data is provided")
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
  missingcol <- missing_columns_check(data, c("sessionId", "factId", "sessionTime", "alpha", "correct"))
  if(length(missingcol) > 0){
    stop("No ", missingcol[[1]] ," column is provided in the data")
  }

  missing_values_message(data, c("sessionId", "factId", "sessionTime", "correct", "alpha"))

  sessionflag <- FALSE

  if(is.null(sessionId)){
    participants <- sort(unique(data$sessionId))
  } else {
    if(!(is.character(sessionId) & length(sessionId) == 1)){
      stop("SessionId is not a string")
    }
    sessionflag <- TRUE
    participants <- character(0)
    participants[1] <- sessionId
  }

  plot <- NULL
  plots <- list()
  plots4 <- list()

  maxTime <- max(data$sessionTime)/60000
  if(is.null(xlim)){
    x = c(0, maxTime)
  } else {
    x = xlim
  }
  if(is.null(ylim)){
    y = c(0.10, 0.5)
  } else {
    y = ylim
  }

  facts <- sort(unique(data$factId))
  factcolor <- viridis::turbo(length(facts))
  names(factcolor)  <- facts

  cat("This may take a moment... \n")
  for (i in seq_along(participants)) {
    dat1 <- dplyr::filter(data, sessionId == participants[i])

    dat3 <- NULL
    if(normalizeTime){
      dat2 <- dplyr::group_by(dat1, factId)
      #when sorted by person, session and factId
      dat3 <- dplyr::mutate(dat2, time = (sessionTime - min(sessionTime)) / 60000)
      dat3 <- dplyr::ungroup(dat3)
    } else {
      dat3 <- dplyr::mutate(dat1, time = (sessionTime / 60000))
    }
    # Make plot title
    lesson <- unique(dat3$lessonTitle)
    user <- unique(dat3$userId)
    plotTitle <- paste("Lesson: ", lesson[1], ", User: ", user[1])

    # Make plot
    plot <- ggplot2::ggplot(data = dat3, ggplot2::aes(x = time, y = alpha)) +
      ggplot2::geom_line(alpha = 1, ggplot2::aes(colour = factor(factId))) +
      ggplot2::geom_point(alpha = 1, size = 1.5, stroke = 0, pch = 21, ggplot2::aes(fill = correct)) +
      ggplot2::guides(colour = "none", fill = "none") +
      ggplot2::scale_fill_manual(values = c("TRUE"="grey", "FALSE"= "red", "1"="grey", "0"= "red")) +
      ggplot2::scale_color_manual(values = factcolor) +
      ggplot2::coord_cartesian(xlim = x, ylim = y) +
      ggplot2::labs(x = "Time (minutes)", y = "Alpha") +
      ggplot2::ggtitle(plotTitle)
    plots[[i]] <- plot
    if(i < 5){
      plots4[[i]] <- plot
    }
  }
  res <- NULL
  if(sessionflag){
    res <- plots[[1]]

    # Save all plots to a pdf file
    title <- paste("ROF_over_time_", title_time(), ".pdf")
    ggplot2::ggsave(title, res, device = "pdf", path = filepath, width = 25, height = 20, units = "cm")

    cat("PDF of plot can be found in: ", filepath, "\n")

  } else {
    res <- cowplot::plot_grid(plotlist = plots4, nrow = 2, ncol = 2)

    # Save all plots to a pdf file
    title <- paste("ROF_over_time_", title_time(), ".pdf")
    ggplot2::ggsave(title, gridExtra::marrangeGrob(grobs = plots, nrow=2, ncol=2),
                    device = "pdf", path = filepath, width = 22, height = 22, units = "cm")

    cat("Preview of the first 4 plots are displayed in viewer. \n")
    cat("PDF of plots can be found in: ", filepath, "\n")
  }

  # Display first 4 plots
  return(res)


}
