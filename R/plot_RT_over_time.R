#' Plot RT over time
#'
#' \code{plot_RT_over_time} plots the RT of one session over time
#' @param data A data frame
#' @param sessionId Provide a single sessionId string to plot that session. If sessionId is NULL all sessions will be plotted.
#' @param normalizeTime If TRUE (the default), the times of all facts will be normalized (they will start at 0). If FALSE, the times will not be normalized and data points will occur relative to their occurrence during the session.
#' @return data frame
#' @export
plot_RT_over_time <- function(data, sessionId = NULL, normalizeTime = TRUE) {
  if(missing(data)){
    stop("No data is provided")
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

  # Change page layout if length(users) =< 2
  plot <- NULL
  plots <- NULL
  for (i in seq_along(participants)) {
    dat1 <- dplyr::filter(data, sessionId == participants[i])
    dat3 <- NULL
    if(normalizeTime){
      dat2 <- dplyr::group_by(dat1, factId)
      #when sorted by person, session and factId
      dat3 <- dplyr::mutate(dat2, time = (sessionTime - min(sessionTime)) / 60000)
    } else {
      dat3 <- dplyr::mutate(dat1, time = (sessionTime / 60000))
    }

    # dat4 <- dplyr::mutate(dat3, reactionTime = reactionTime / 1000)  ReactionTime in seconds
    maxTime <- max(dat3$time)
    meanRT <- mean(dat3$reactionTime)
    sdRT <- sd(dat3$reactionTime)
    upperRT <- meanRT + (2*sdRT)
    lowerRT <- min(dat3$reactionTime)

    plot <- ggplot2::ggplot(data = dat3, ggplot2::aes(x = time, y = reactionTime)) +
      ggplot2::geom_line(alpha = 1, ggplot2::aes(colour = factor(factId))) +
      ggplot2::geom_point(alpha = 0.5, size = 1, ggplot2::aes(colour = factor(factId), fill = factor(factId))) +
      ggplot2::guides(colour = "none", fill = "none") +
      # ggplot2::scale_x_continuous(breaks = seq(0, 2.6, 1), limits = c(0, 2.6), minor_breaks = NULL) +
      # ggplot2::scale_y_continuous(breaks = seq(100, 3100, 500), limits = c(100, 3100), minor_breaks = NULL) +
      ggplot2::scale_color_viridis_d() +
      ggplot2::scale_fill_viridis_d() +
      ggplot2::coord_cartesian(xlim = c(0, maxTime), ylim = c(lowerRT, upperRT)) +
      ggplot2::labs(x = "Time (minutes)", y = "Reaction Time (ms)")
    title <- paste("Plot", i,".pdf")
    ggplot2::ggsave(title, plot = plot, path = "C:/Users/Hanna/OneDrive/Documenten/Werk/SlimStampen/Package R/Figures")

  }
  plot


  # dat1 <- dplyr::filter(data, userId == 60168 & sessionId == "")
  # dat2 <- dplyr::group_by(dat1, factId)
  # #when sorted by person, session and factId
  # dat3 <- dplyr::mutate(dat2, time = (sessionTime - min(sessionTime)) / 60000)
  # # dat4 <- dplyr::mutate(dat3, reactionTime = reactionTime / 1000)  ReactionTime in seconds
  #
  # ggplot2::ggplot(data = dat3, ggplot2::aes(x = time, y = reactionTime)) +
  #   ggplot2::geom_line(alpha = 1, ggplot2::aes(colour = factor(factId))) +
  #   ggplot2::geom_point(alpha = 0.5, size = 1, ggplot2::aes(colour = factor(factId), fill = factor(factId))) +
  #   ggplot2::guides(colour = "none", fill = "none") +
  #   ggplot2::scale_x_continuous(breaks = seq(0, 2.6, 1), limits = c(0, 2.6), minor_breaks = NULL) +
  #   ggplot2::scale_y_continuous(breaks = seq(100, 3100, 500), limits = c(100, 3100), minor_breaks = NULL) +
  #   ggplot2::scale_color_viridis_d() +
  #   ggplot2::scale_fill_viridis_d() +
  #   ggplot2::labs(x = "Time (minutes)", y = "Reaction Time (ms)")
}
