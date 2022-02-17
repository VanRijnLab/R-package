#' Plot RT over time
#'
#' \code{plot_RT_over_time} plots the RT of one session over time. Incorrect
#' answers are denotes with a red marker.
#'
#' @param data A data frame. NA values will be removed before plotting.
#' @param sessionId Provide a single sessionId string to plot that session. If
#'   sessionId is NULL all sessions will be plotted.
#' @param normalizeTime If TRUE, the times of all facts will be normalized (they
#'   will start at 0). If FALSE, the times will not be normalized and data
#'   points will occur relative to their occurrence during the session.
#' @param xlim A vector of 2 (for example: c(0, 10)), indicating the range of
#'   the x-axis.If NULL the default value is used: c(0, max(time)).
#' @param ylim A vector of 2 (for example: c(0, 10)), indicating the range of
#'   the y-axis.If NULL the default value is used: c(min(RT), mean(RT) +
#'   (2*SD(RT))).
#' @param filepath A relative or explicit path where plots will be saved
#' @return data frame
#' @export
plot_RT_over_time <- function(data, sessionId = NULL, normalizeTime = FALSE, xlim = NULL, ylim = NULL, filepath = "../Figures") {
  if(missing(data)){
    stop("No data is provided")
  }
  if(!(is.null(xlim) | length(xlim) == 2)){
    stop("xlim must be a vector of 2")
  }
  if(!(is.null(ylim) | length(ylim) == 2)){
    stop("ylim must be a vector of 2")
  }
  indx <- apply(data, 2, function(x) any(is.na(x)))
  if("reactionTime" %in% colnames(data)[indx]){
    cat("! There are missing values in the column reactionTime. \n")
  }
  if("sessionTime" %in% colnames(data)[indx]){
    cat("! There are missing values in the column sessionTime. \n")
  }
  if("factId" %in% colnames(data)[indx]){
    cat("! There are missing values in the column factId. \n")
  }
  if("correct" %in% colnames(data)[indx]){
    cat("! There are missing values in the column correct. \n")
  }

  if(is.null(sessionId)){
    participants <- unique(data$sessionId)
  } else {
    if(!(is.character(sessionId) & length(sessionId) == 1)){
      stop("SessionId is not a string")
    }
    participants <- character(0)
    participants[1] <- sessionId
  }

  plot <- NULL
  plots <- list()
  plots4 <- list()

  maxTime <- max(data$sessionTime)/60000
  meanRT <- mean(data$reactionTime, na.rm=TRUE)
  sdRT <- stats::sd(data$reactionTime, na.rm=TRUE)
  upperRT <- meanRT + (sdRT)
  lowerRT <- min(data$reactionTime)
  if(is.null(xlim)){
    x = c(0, maxTime)
  } else {
    x = xlim
  }
  if(is.null(ylim)){
    y = c(lowerRT, upperRT)
  } else {
    y = ylim
  }

  facts <- unique(data$factId)
  factcolor <- viridis::turbo(length(facts))
  # factcolor <- grDevices::rainbow(length(facts))
  names(factcolor)  <- facts

  corrections <- unique(data$correct)
  corcolor <- c("grey", "red")
  names(corcolor)  <- corrections


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
    plot <- ggplot2::ggplot(data = dat3, ggplot2::aes(x = time, y = reactionTime)) +
      ggplot2::geom_line(alpha = 1, ggplot2::aes(colour = factor(factId))) +
      ggplot2::geom_point(alpha = 1, size = 1.5, stroke = 0, pch = 21, ggplot2::aes(fill = correct)) +
      ggplot2::guides(colour = "none", fill = "none") +
      ggplot2::scale_fill_manual(values = corcolor) +
      ggplot2::scale_color_manual(values = factcolor) +
      ggplot2::coord_cartesian(xlim = x, ylim = y) +
      ggplot2::labs(x = "Time (minutes)", y = "Reaction Time (ms)") +
      ggplot2::ggtitle(plotTitle)
    plots[[i]] <- plot
    if(i < 5){
      plots4[[i]] <- plot
    }
  }
  res <- cowplot::plot_grid(plotlist = plots4, nrow = 2, ncol = 2)

  # Save all plots to a pdf file
  date <- format(Sys.time(), "%d-%b-%Y %Hh%Mm")
  title <- paste("RT_over_time_", date, ".pdf")
  ggplot2::ggsave(title, gridExtra::marrangeGrob(grobs = plots, nrow=2, ncol=2),
         device = "pdf", path = filepath, width = 22, height = 22, units = "cm")

  cat("Preview of the first 4 plots are displayed in viewer. \n")
  cat("PDF of plots can be found in: ", filepath, "\n")

  # Display first 4 plots
  return(res)
}
