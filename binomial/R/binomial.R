# checking invadility of a probability input
check_prob <- function(prob = 0.5) {
  if (prob >= 0 & prob <= 1) {
    TRUE
  } else {
    stop("invalid prob value: p has to be a number betwen 0 and 1")
  }
}



# checking invadility of a trial input
check_trials <- function(trials = 5) {
  if (trials >= 0 & trials %% 1 == 0) {
    TRUE
  } else {
    stop("invalid trials value: trial have to be a non-negative integer")
  }
}


# checking invadility of a success input
check_success <- function(trials = 5, success = 1) {
  check_trials(trials)
  if (any(trials >= success & success %% 1 == 0)) {
    TRUE
  } else if (any(trials < success)) {
    stop("invalid success value: success cannot be greater than trials")
  } else if (any(success < 0 | success %% 1 != 0)) {
    stop("invalid success value: success have to be a non-negative integer")
  }
}



# auxiliary function for bin_mean()
aux_mean <- function(trials = 5, prob = 0.5) {
  trials * prob
}



# auxiliary function for bin_variance()
aux_variance <- function(trials = 5, prob = 0.5) {
  (trials * prob) * (1 - prob)
}



# auxiliary function for bin_mode()
aux_mode <- function(trials = 5, prob = 0.5) {
  if((trials * prob + prob) %% 1 != 0) {
    (trials * prob + prob) - ((trials * prob + prob) %% 1)
  } else if ((trials * prob + prob) %% 1 == 0) {
    c((trials * prob + prob), (trials * prob + prob - 1))
  }
}



# auxiliary function for bin_skewness()
aux_skewness <- function(trials = 5, prob = 0.5) {
  (1 - 2 * prob) / (sqrt((trials * prob) * (1 - prob)))
}



# auxiliary function for bin_kurtosis()
aux_kurtosis <- function(trials = 5, prob = 0.5) {
  (1 - 6 * prob * (1 - prob)) / ((trials * prob) * (1 - prob))
}




#' @title bin_choose
#' @description the number of combinations in which k successes can occur in n trials
#' @param n (numeric)
#' @param k (numeric)
#' @return the number of combinations
bin_choose <- function(n = 5, k = 2) {
  f = (factorial(n)) / (factorial(k) * factorial(n - k))
  ifelse(k > n, stop("k cannot be greater than n"), f)
}


#' @title bin_probability
#' @description the probability in which k successes can occur in n trials with probability p for each success to happen
#' @param trials (numeric)
#' @param success (numeric)
#' @param prob (numeric)
#' @return the probability value
bin_probability <- function(trials = 5,success = 1, prob = 0.5) {
  check_trials(trials)
  check_prob(prob)
  check_success(trials = trials, success = success)
  bin_choose(trials, success) * prob ^ (success) * (1 - prob) ^ (trials - success)
}



#' @title bin_distribution
#' @description the probability distribution over successes
#' @param trials (numeric)
#' @param prob (numeric)
#' @return data.frame
bin_distribution <- function(trials = 5, prob = 0.5) {
  bindis <- data.frame("success" = 0 : trials, "probability" = 0 : trials)
  for (i in 1: nrow(bindis))
    bindis[i, 2] <- bin_probability(success = i - 1, trials = trials, prob = prob)
  class(bindis) <- c("bindis", "data.frame")
  bindis
}




#' @title plot.bindis
#' @export plot
plot.bindis <- function(x, ...) {
  library(ggplot2)
  ggplot(x, aes(success, probability)) + geom_histogram(stat = "identity", alpha = 0.7) + theme_classic() + xlab("successes")
}





#' @title  bin_cumulative
#' @description the cumulative probability distribution over successes
#' @param trials (numeric)
#' @param prob (numeric)
#' @return data.frame
bin_cumulative <- function(trials = 5, prob = 0.5) {
  bincum <- bin_distribution(trials = trials, prob = prob)
  bincum$cumulative <- 0: trials
  bincum[1, 3] <- bincum[1,2]
  for (i in 2: nrow(bincum))
    bincum[i, 3] <- bincum[i, 2] + bincum[i - 1,3]
  class(bincum) <- c("bincum", "data.frame")
  bincum
}




#' @title plot.bincum
#' @export plot
plot.bincum <- function(x, ...) {
  plot(x$success, x$cumulative, type = "l", xlab = "successes", ylab = "probability")
  points(x$success, x$cumulative)
}




#' @title  bin_variable
#' @description the binomial random variable object
#' @param trials (numeric)
#' @param prob (numeric)
#' @return a list of information about the object
bin_variable <- function(trials = 5, prob = 0.5) {
  check_trials(trials)
  check_prob(prob)
  binvar <- list(trials = trials, prob = prob)
  class(binvar) <- "binvar"
  binvar
}



#' @title print.binvar
#' @export print.binvar print the information about binvar
print.binvar <- function(x, ...) {
  cat('"Binomial variable"\n\n')
  cat("Parameters\n")
  cat("- number of trials:", x$trials, "\n")
  cat("- prob of succes:", x$prob, "\n")
  invisible(x)
}




#' @title summary.binvar
#' @export summary.binvar more detailed information about binvar
summary.binvar <- function(x, ...) {
  binvar <- list(trials = x$trials,
                 prob = x$prob,
                 mean = aux_mean(x$trials, x$prob),
                 variance = aux_variance(x$trials, x$prob),
                 mode = aux_mode(x$trials, x$prob),
                 skewness = aux_skewness(x$trials, x$prob),
                 kurtosis = aux_kurtosis(x$trials, x$prob))
  class(binvar) <- "summary.binvar"
  binvar
}




#' @title print.summary.binvar
#' @export print.summary.binvar print more detailed information about binvar
print.summary.binvar <- function(x, ...) {
  cat('"Binomial variable"\n\n')
  cat("Parameters\n")
  cat("- number of trials: ", x$trials, "\n")
  cat("- prob of success:", x$prob, "\n\n")
  cat("Measures\n")
  cat("- mean:", x$mean, "\n")
  cat("- variance:", x$variance, "\n")
  cat("- mode:", x$mode, "\n")
  cat("- skewness:", x$skewness, "\n")
  cat("- kurtosis:", x$kurtosis, "\n")
}




#' @title  bin_mean
#' @description the expect number of a binomial distribution
#' @param trials (numeric)
#' @param prob (numeric)
#' @return the expected value
bin_mean <- function(trials = 5, prob = 0.5) {
  check_trials(trials)
  check_prob(prob)
  aux_mean(trials, prob)
}




#' @title  bin_variance
#' @description the variance of the binomial distribution
#' @param trials (numeric)
#' @param prob (numeric)
#' @return the variance value
bin_variance <- function(trials = 5, prob = 0.5) {
  check_trials(trials)
  check_prob(prob)
  aux_variance(trials, prob)
}



#' @title  bin_mode
#' @description the mode of the binomial distribution
#' @param trials (numeric)
#' @param prob (numeric)
#' @return the mode value
bin_mode <- function(trials = 5, prob = 0.5) {
  check_trials(trials)
  check_prob(prob)
  aux_mode(trials, prob)
}



#' @title  bin_skewness
#' @description the skewness of the binomial distribution
#' @param trials (numeric)
#' @param prob (numeric)
#' @return the skewness value
bin_skewness <- function(trials = 5, prob = 0.5) {
  check_trials(trials)
  check_prob(prob)
  aux_skewness(trials, prob)
}



#' @title  bin_kurtosis
#' @description the kurtosis of the binomial distribution
#' @param trials (numeric)
#' @param prob (numeric)
#' @return the kurtosis value
bin_kurtosis <- function(trials = 5, prob = 0.5) {
  check_trials(trials)
  check_prob(prob)
  aux_kurtosis(trials, prob)
}





