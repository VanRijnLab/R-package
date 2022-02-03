#' Plot RT over time
#'
#' \code{plot_RT_over_time} plots the RT of one person of one session over time
#' @param data data frame
#' @return data frame
#' @export
plot_RT_over_time <- function(data) {
  dat1 <- dplyr::filter(data, userId == 60168 & sessionId == "7f27ee3c-2e6b-42a4-aff1-ba2492008ab6")
  dat2 <- dplyr::group_by(dat1, factId)
  #when sorted by person, session and factId
  dat3 <- dplyr::mutate(dat2, time = (sessionTime - min(sessionTime)) / 60000)
  # dat4 <- dplyr::mutate(dat3, reactionTime = reactionTime / 1000)  ReactionTime in seconds

  ggplot2::ggplot(data = dat3, ggplot2::aes(x = time, y = reactionTime)) +
    ggplot2::geom_line(alpha = 1, ggplot2::aes(colour = factor(factId))) +
    ggplot2::geom_point(alpha = 0.5, size = 1, ggplot2::aes(colour = factor(factId), fill = factor(factId))) +
    ggplot2::guides(colour = "none", fill = "none") +
    ggplot2::scale_x_continuous(breaks = seq(0, 2.6, 1), limits = c(0, 2.6), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(breaks = seq(100, 3100, 500), limits = c(100, 3100), minor_breaks = NULL) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::labs(x = "Time (minutes)", y = "Reaction Time (ms)")
}
