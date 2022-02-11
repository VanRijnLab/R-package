#' Descriptive statistics about the data set
#'
#' \code{dataset_stats} describes several statistics regarding the data set,
#' these are: Number of participants, number of unique facts, number of lessons,
#' number of unique user-fact combinations, participants per lesson, facts per
#' lesson, unique user-fact combinations per lesson, lessons per participant and
#' sessions per participant.
#'
#' The number of participants, number of unique facts, number of lessons and
#' participants per lesson are always printed in the console. The data frame
#' with the sessions and lessons per participants is not printed, since the
#' amount of participants can be substantial. All information can be found in
#' $general, $lessons and $participants.
#'
#' @param data A data frame
#'
#' @return Three data frames ($general, $lessons, $participants)
#' @export
#'
dataset_stats <- function(data) {
  if(missing(data)){
    stop("No data is provided")
  }
  participants = unique(data$userId)
  nrparticipants = length(participants)

  nrfacts = length(unique(data$factId))

  lessons = unique(data$lessonTitle)
  nrlessons = length(lessons)

  nrfactsuser = dplyr::n_distinct(paste(data$factId, data$userId))

  partPerLesson = integer(0)
  factsPerLesson = integer(0)
  userfactsPerLesson = integer(0)
  for (i in seq_along(lessons)) {
    dat1 <- dplyr::filter(data, lessonTitle == lessons[i])
    partPerLesson[i] <- length(unique(dat1$userId))
    factsPerLesson[i] <- length(unique(dat1$factId))
    userfactsPerLesson[i] <- dplyr::n_distinct(paste(dat1$factId, dat1$userId))
  }
  sessionPerPart = integer(0)
  lessonPerPart = integer(0)
  factsPerPart = integer(0)
  for (i in seq_along(participants)) {
    dat1 <- dplyr::filter(data, userId == participants[i])
    sessionPerPart[i] <- length(unique(dat1$sessionId))
    lessonPerPart[i] <- length(unique(dat1$lessonId))
    factsPerPart[i] <- length(unique(dat1$factId))
  }
  out <- list()
  out$general <- data.frame(descriptives = c("Total participants", "Total facts", "Total lessons", "Fact-user combinations"),
                   values = c(nrparticipants, nrfacts, nrlessons, nrfactsuser))
  out$lessons <- data.frame(lessons = lessons,
                    participants = partPerLesson, facts = factsPerLesson, user_facts = userfactsPerLesson)
  out$participants <- data.frame(participant = participants,
                    sessions = sessionPerPart, lessons = lessonPerPart, facts = factsPerPart)
  cat("General stats ($general): \n")
  print(out$general)
  cat("\n Information per lesson ($lessons): \n")
  print(out$lessons)
  cat("\n The information per participant can be found in $participants")

  invisible(out)
}
