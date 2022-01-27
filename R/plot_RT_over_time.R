#' Plot RT over time
#'
#' \code{plot_RT_over_time} plots the RT of one person of one session over time
#' @param data data frame
#'
#' @return data frame
#' @export
plot_RT_over_time <- function(data) {
  data %>%
    dplyr::filter(userId == 54911 & sessionId == "83f4d01f-805c-41b1-a677-56e08ec95576")
  # dat1 <- dplyr::filter(data, userId == 54911 & sessionId == "83f4d01f-805c-41b1-a677-56e08ec95576")
  # dat2 <- dplyr::group_by(dat1, factId)
  # #when sorted by person, session and factId
  # dat2$time <- (dat2$sessionTime - min(dat2$sessionTime)) / 60000
  # ggplot(data = spec_data, aes(time, rt, color = block)) + geom_smooth() +
  #   ggtitle("Reaction times over time") +  labs(x="Time (min)", y="RT (ms)")
}
