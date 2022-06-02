#' Calculate fact activation and alpha (rate of forgetting)
#'
#' Returns the data frame with a column for activation and alpha for all trials. This will
#' override existing columns called 'alpha' or 'activation'.
#'
#' @family calculation functions
#'
#' @param data A data frame
#' @param minAlpha The minimum value that the alpha can be
#' @param maxAlpha The maximum value that the alpha can be
#' @param fThreshold Threshold below which the user forgets a fact
#' @param standardAlpha The default alpha that all items start with
#'
#' @return Original data frame with columns for activation and alpha
#' @export
#'
calculate_alpha_and_activation <- function(data, minAlpha = 0.15,
                                           maxAlpha = 0.5, fThreshold = -0.8, standardAlpha = 0.3) {
  if(missing(data)){
    stop("No data is provided")
  }
  missingcol <- missing_columns_check(data, c("sessionId", "factId", "factText", "userId", "presentationStartTime","reactionTime", "correct"))
  if(length(missingcol) > 0){
    stop("No ", missingcol[[1]] ," column is provided in the data")
  }
  missing_values_message(data, c("sessionId", "factId", "factText", "userId", "presentationStartTime","reactionTime", "correct"))

  if(-1 %in% data$factId){
    data <- resetremoval(data)
    cat("- There are resets present in the data. Reset data is excluded in this function. - \n")
  }

  cat("This may take a few minutes for large datasets... \n")


  participants <- unique(data$sessionId)
  datalistTotal = list()

  for (j in seq_along(participants)) {
    datalistPar = list()
    dat1 <- dplyr::filter(data, sessionId == participants[j])
    facts <- unique(dat1$factId)
    dat1 <- dplyr::arrange(.data = dat1, presentationStartTime)
    dat2 <- dplyr::mutate(.data = dat1, .keep = "none", threshold = fThreshold,
                          fact_id = factId, text = factText, start_time = presentationStartTime,
                          rt = reactionTime, correct = correct)
    for (i in seq_along(facts)) {
      datfact <- dplyr::filter(dat1, factId == facts[i])
      alpha_activation <- calculate_activation_and_alpha_all(id = facts[i], factalpha = standardAlpha,
                                                             responses = dat2, min_alpha = minAlpha,
                                                             max_alpha = maxAlpha)
      datfact$alpha <- alpha_activation$alpha
      datfact$activation <- alpha_activation$activation
      datalistPar[[i]] <- datfact
    }
    datParticipant <- data.table::rbindlist(datalistPar)
    datalistTotal[[j]] <- datParticipant
  }
  datTotal <- data.table::rbindlist(datalistTotal)
  datSortTime <- datTotal[with(datTotal, order(presentationStartTime)),]
  datSortParticipant <- datSortTime[with(datSortTime, order(sessionId)),]
  cat("Done! \n")
  return(datSortParticipant)

}


# Calculate fact activation and rate of forgetting.
#
# Returns the activation and rate of forgetting of a fact for at the start of
# each trial in a set of responses
#
# @param id The identifier of the fact.
# @param factalpha The initial value of the rate of forgetting.
# @param responses A data frame containing responses, with columns fact_id,
#   text, start_time, rt, correct, threshold.
# @param min_alpha Lower bound on rate of forgetting.
# @param max_alpha Upper bound on rate of forgetting.
# @return A data.table containing the activation (a double) and the rate of
#   forgetting (a double) for each trial.
#
calculate_activation_and_alpha_all <- function (id, factalpha, responses, min_alpha, max_alpha) {
  responses_for_fact <- dplyr::filter(responses, fact_id == id)
  n_responses <- nrow(responses_for_fact)
  encounters <- list()
  activation_and_alpha <- list()
  alpha <- factalpha

  if (n_responses > 0) {
    for (i in seq_len(n_responses)) {
      activation <- calculate_activation_from_encounters(encounters, responses_for_fact$start_time[i])
      encounters[[i]] <- list(activation = activation,
                              time = responses_for_fact$start_time[i],
                              rt = normalise_reaction_time(responses_for_fact[i,]),
                              decay = NA)
      alpha <- estimate_alpha(encounters, activation, responses_for_fact[i,], alpha, factalpha)

      alpha <- normalise_alpha(alpha, min_alpha, max_alpha)

      for (j in 1:length(encounters)) {
        encounters[[j]]$decay <- calculate_decay(encounters[[j]]$activation, alpha)
      }

      activation <- calculate_activation_from_encounters(encounters, responses_for_fact$start_time[i])

      activation_and_alpha[[i]] <- list(activation = activation, alpha = alpha)
    }
  }

  return (data.table::rbindlist(activation_and_alpha))
}


calculate_activation_from_encounters <- function (encounters, time) {
  sum <- 0
  if (length(encounters) > 0) {
    for (i in 1:length(encounters)) {
      if (encounters[[i]]$time < time) {
        sum <- sum + ((time - encounters[[i]]$time) / 1000) ^ -encounters[[i]]$decay
      }
    }
  }
  return (log(sum))
}





estimate_alpha <- function (encounters, activation, response, previous_alpha, factalpha) {
  if (length(encounters) < 3) {
    return (factalpha)
  }

  a0 <- -Inf
  a1 <- Inf
  a_fit <-previous_alpha
  reading_time <- get_reading_time(response$text)
  estimated_rt <- estimate_reaction_time_from_activation(activation, reading_time)
  est_diff <- estimated_rt - normalise_reaction_time(response)

  if (est_diff < 0) {
    a0 <- a_fit
    a1 <- a_fit + 0.05
  } else {
    a0 <- a_fit - 0.05
    a1 <- a_fit
  }

  for (j in 1:6) {
    a0_diff = a0 - a_fit
    a1_diff = a1 - a_fit
    d_a0 = purrr::map(encounters, function(enc) {
      return(list(activation = enc$activation,
                  decay = enc$decay + a0_diff,
                  rt = enc$rt,
                  time = enc$time))
    })
    d_a1 = purrr::map(encounters, function(enc) {
      return(list(activation = enc$activation,
                  decay = enc$decay + a1_diff,
                  rt = enc$rt,
                  time = enc$time))
    })

    encounter_window <- encounters[max(2, length(encounters) - 4):length(encounters)]

    total_a0_error <- calculate_predicted_reaction_time_error(encounter_window, d_a0, reading_time)
    total_a1_error <- calculate_predicted_reaction_time_error(encounter_window, d_a1, reading_time)

    ac <- (a0 + a1) / 2

    if (total_a0_error < total_a1_error) {
      a1 <- ac
    } else {
      a0 <- ac
    }

  }

  return ((a0 + a1) / 2)
}





calculate_predicted_reaction_time_error <- function (test_set, decay_adjusted_encounters, reading_time) {
  error <- 0
  for (i in 1:length(test_set)) {
    enc <- test_set[[i]]
    m <- calculate_activation_from_encounters(decay_adjusted_encounters, enc$time - 100)
    rt <- estimate_reaction_time_from_activation(m, reading_time)
    error <- error + abs(enc$rt - rt)
  }
  return (error)
}





calculate_decay <- function (activation, alpha) {
  return (0.25 * exp(activation) + alpha)
}





normalise_alpha <- function (alpha, min_alpha, max_alpha) {
  if (!is.na(max_alpha) && alpha > max_alpha) {
    return (max_alpha)
  } else if (!is.na(min_alpha) && alpha < min_alpha) {
    return (min_alpha)
  }
  return (alpha)
}





normalise_reaction_time <- function (response) {
  reaction_time <- dplyr::if_else(response$correct == 1 && !is.na(response$rt), as.double(response$rt), 60000)
  max_reaction_time <- get_max_reaction_time_for_fact(response$text, response$threshold)
  return(dplyr::if_else(reaction_time > max_reaction_time, max_reaction_time, reaction_time))
}





get_max_reaction_time_for_fact <- function (text, threshold) {
  reading_time <- get_reading_time(text)
  return(1.5 * estimate_reaction_time_from_activation(threshold, reading_time))
}





get_reading_time <- function (text) {
  word_count = length(strsplit(text, " ")[[1]])
  if (word_count > 1) {
    character_count <- nchar(text)
    return(max((-157.9 + character_count * 19.5), 300))
  }
  return(300)
}





estimate_reaction_time_from_activation <- function (activation, reading_time) {
  return((1.0 * exp(-activation) + (reading_time / 1000)) * 1000)
}
