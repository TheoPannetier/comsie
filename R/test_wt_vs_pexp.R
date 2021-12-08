#' Run a one-sample KS test on a sample of waiting times
#'
#' For each value of N and speciation or exinction events,
#' fits an exponential distribution (with ML) to the waiting times and run a
#' Kolmogorov-Smirnov test of the waiting times vs the theoretical exponential distribution.
#'
#' @param waiting_times_tbl a `tibble`, the output of [waiting_times()] for one
#' or more trees.
#'
#' @return a `tibble` with:
#'
#' - `N` standing diversity
#' - `event` either speciation or extinction
#' - `n` number of observations, i.e. waiting times
#' - `loglik` log-likelihood of the exponential with ML estimate `rate`
#' - `rate` ML estimate of the exponential dist. rate parameter
#' - `stat` Kolmogorov-Smirnov distance
#' - `p` p-value of the test
#' - `test` type of KS test
#'
#' @author Theo Pannetier
#' @export
test_wt_vs_pexp <- function(waiting_times_tbl) {

  Ns <- unique(waiting_times_tbl$N)

  fit_tbl <- purrr::map_dfr(Ns, function(n) {
    purrr::map_dfr(list("speciation", "extinction"), function(event) {

      wt <- waiting_times_tbl %>%
        dplyr::filter(N == n, next_event == event) %>%
        dplyr::pull(waiting_time)

      if (length(wt) == 0) {
        fit_tbl <- tibble::tibble(
          "N" = n,
          "event" = event,
          "rate" = NA,
          "n" = 0,
          "loglik" = NA,
          "stat" = NA,
          "p" = NA,
          "test" = NA
        )
      } else {
        fit <- MASS::fitdistr(x = wt, densfun = "exponential")
        test <- stats::ks.test(x = wt, y = "pexp", fit$estimate)
        fit_tbl <- tibble::tibble(
          "N" = n,
          "event" = event,
          "rate" = fit$estimate,
          "n" = fit$n,
          "loglik" = fit$loglik,
          "stat" = test$statistic,
          "p" = test$p.value,
          "test" = test$method
        )
      }
      return(fit_tbl)
    })
  })
}
