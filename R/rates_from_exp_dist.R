#' Estimate diversity-dependent rates from mean parameter of the exponential distribution
#'
#' For a list of phylogenies, reconstruct the *average, per-capita* rates of
#' speciation and extinction corresponding to each level of species diversity.
#'
#' @param waiting_times_tbl a table with waiting times, the output of
#' [waiting_times()] for one or more phylogenies.
#'
#' @return a `tibble` with a row for every possible number of species and 4
#' columns:
#'
#' * `N`, level of diversity, nb of species
#' * `rate` (speciation or extinction)
#' * `value` value of that rate
#' * `dd_model` = `none`, for compatibility with [rates_from_dd_model()]
#' * `ci_upper` upper bound of the confidence interval of `value`
#' * `ci_lower` lower bound of the confidence interval of `value`
#'
#' @return
#' @author Théo Pannetier
#' @export
#'

rates_from_exp_dist <- function(waiting_times_tbl) {
  # Prop of speciation/extinction events in each N bin
  p_tbl <- waiting_times_tbl %>%
    dplyr::group_by(N, next_event) %>%
    dplyr::summarise(
      "n_events" = dplyr::n()
    ) %>%
    dplyr::mutate(
      "p_event" = n_events / sum(n_events)
    ) %>%
    dplyr::rename("rate" = next_event)

  # Compute rates pooled as (spec_rate + ext_rate)
  rates_tbl <- waiting_times_tbl %>%
    dplyr::group_by(N) %>%
    dplyr::summarise(
      "mean_waiting_time" = mean(waiting_time, na.rm = TRUE),
      "n" = n()
    ) %>%
    dplyr::mutate(
      "total_event_rate" = 1 / (mean_waiting_time * N)
    ) %>%
    # Split speciation and extinction rates by proportion
    tidyr::expand_grid("rate" = c("speciation", "extinction")) %>%
    dplyr::left_join(p_tbl, by = c("N", "rate")) %>%
    dplyr::transmute(
      "N" = N,
      "dd_model" = "none",
      "rate" = rate,
      "value" = total_event_rate * p_event,
      # 95% confidence interval of exponential distribution
      "ci_upper" = value * (1 + 1.96 / sqrt(n * p_event)),
      "ci_lower" = value * (1 - 1.96 / sqrt(n * p_event))
    )
  # Values of N with no extinction events should have extinction rate = 0
  rates_tbl <- rates_tbl %>%
    dplyr::mutate(
      "value" = ifelse(rate == "extinction" & is.na(value), 0, value),
      "ci_upper" = ifelse(rate == "extinction" & is.na(ci_upper), 0, ci_upper),
      "ci_lower" = ifelse(rate == "extinction" & is.na(ci_lower), 0, ci_lower)
    )
  return(rates_tbl)
}
