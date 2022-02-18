#' Calculate fact repetition
#'
#' Returns the data frame with fact repetition for all trials.
#'
#' @param data A data frame
#'
#' @return Original data frame with column for repetition
#' @export
#'
calculate_repetition <- function(data) {
  if(missing(data)){
    stop("No data is provided")
  }
  cat("This may take a moment... \n")

  participants <- unique(data$sessionId)
  datalistTotal = list()

  for (j in seq_along(participants)) {
    datalistPar = list()
    dat1 <- dplyr::filter(data, sessionId == participants[j])
    facts <- unique(dat1$factId)
    dat1 <- dplyr::arrange(.data = dat1, sessionTime)
    for (i in seq_along(facts)) {
      datfact <- dplyr::filter(dat1, factId == facts[i])
      datfact <- dplyr::mutate(.data = datfact, repetition = 0:(dplyr::n()-1))
      datalistPar[[i]] <- datfact
    }
    datParticipant <- data.table::rbindlist(datalistPar)
    datalistTotal[[j]] <- datParticipant
  }
  datTotal <- data.table::rbindlist(datalistTotal)
  datSortTime <- datTotal[with(datTotal, order(sessionTime)),]
  datSortParticipant <- datSortTime[with(datSortTime, order(sessionId)),]
  cat("Done! \n")
  return(datSortParticipant)

}
