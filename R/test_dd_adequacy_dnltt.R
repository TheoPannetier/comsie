#' Run a robustness test as a measure of absolute fit
#'
#' This function will run a test that is meant as a measure of the absolute fit
#' of a DD model to a set of comrad trees. The test uses the delta nLTT statistic
#' (Janzen et al. 2015) as a measure of error between the branching pattern expected under a
#' DD model and that observed in a set of comrad trees. Inspired from the bootstrap tests
#' of Louca & Pennell (2021) and Santos Neves, Lambert et al. (2021). Deterministic LTT of the DD
#' model estimated as the average LTT of the bootstrap set.
#'
#' Here, "empirical trees" is meant as a set of trees to which the fit of a DD model is to be assessed,
#' while "bootstrap trees" are a set of trees simulated under the DD model, with maximum likelihood estimates for the
#' set of empirical trees.
#'
#' @param ltts_empirical a list of `tibble` with columns `time` and `N` (as produced by [get_ltt_tbl()])
#' containing the LTTs of the set of empirical trees.
#' @param ltts_bootstrap a list of `tibble` with columns `time` and `N` (as produced by [get_ltt_tbl()])
#' containing the LTTs of the set of bootstrap trees.
#'
#' @return a list with four elements:
#' - `dnltt_tbl` a tibble with variables `dnltt` (values of the delta nLTT) and `set` (`"empirical"` or `"bootstrap"`)
#' - `avg_ltt_bootstrap` a `tibble` containing the average LTT of the bootstrap set.
#' - `p_95` a measure akin a p-value, the proportion of empirical trees with a delta nLTT higher than the 95th percentile of the distribution delta nLTT of the bootstrap test.
#' Based on the $p_95$ of Santos Neves, Lambert et al. (2021).
#' - `ks` output of a two-sample, one-sided (empirical > bootstrap) Kolmogorv-Smirnov test on the distribution of the delta nLTT of the empirical and bootsrap test.
#'
#' @export
#' @author Theo Pannetier

test_dd_adequacy_dnltt <- function(ltts_empirical, ltts_bootstrap) {

  # Compute average LTT of the bootstrap test, estimate of the deterministic LTT of the model
  t_seq <- seq(min(ltts_empirical[[1]]$time), 0, 100)
  avg_ltt_bootstrap <- ltts_bootstrap %>%
    average_ltt(t_seq)

  # Compute delta nLTT vs reference LTT for the trees of each set
  dnltts_bootstrap <- ltts_bootstrap %>% purrr::map_dbl(get_dnltt, avg_ltt_bootstrap)
  dnltts_empirical <- ltts_empirical %>% purrr::map_dbl(get_dnltt, avg_ltt_bootstrap)

  pvals <- dnltts_empirical %>% purrr::map_dbl(function(dnltt_empirical) {
    sum(dnltts_bootstrap > dnltt_empirical) / length(dnltts_bootstrap)
  })

  # Get decision criteria
  dnltt_95th <- stats::quantile(dnltts_bootstrap, probs = 0.95)
  p_95 <- 1 - sum(dnltts_empirical > dnltt_95th) / length(dnltts_empirical)
  ks_test <- stats::ks.test(dnltts_empirical, dnltts_bootstrap, alternative = "less")

  dnltt_tbl <- dplyr::bind_rows(
    tibble::tibble("dnltt" = dnltts_empirical, "set" = "empirical"),
    tibble::tibble("dnltt" = dnltts_bootstrap, "set" = "bootstrap")
  )

  return(list(
    "dnltt_tbl" = dnltt_tbl,
    "avg_ltt_bootstrap" = avg_ltt_bootstrap,
    "p_95" = p_95,
    "ks_test" = ks_test
  ))
}
