#' Compute the average LTT with 95% CI for a set of trees given a time sequence
#'
#' @param ltt_tbls a list of `tibble` that contain the LTT of the trees with columns
#' `time` and `N`, e.g. the output of [get_ltt_tbl()].
#' @param t_seq a vector of doubles containing the times at which the average N
#' should be evaluated.
#'
#' @return a `tibble` with columns `time`, `N`, `metric`. Values are the mean
#' number of species, standard deviation, SEM, and 95% upper and lower CI.
#' @author Theo Pannetier
#' @export
average_ltt <- function(ltt_tbls, t_seq) {

  ltt_tbl <- ltt_tbls %>%
    purrr::map_dfr(interpolate_ltt, t_seq) %>%
    dplyr::group_by(time)

  summary_tbl <- ltt_tbl %>%
    dplyr::summarise(
      "mean" = mean(N),
      "sd" = sd(N),
      "sem" = sd / sqrt(n()),
      "upper_ci_95" = mean + 1.96 * sem,
      "lower_ci_95" = mean - 1.96 * sem,
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = mean:lower_ci_95,
      names_to = "metric",
      values_to = "N"
    )
  return(summary_tbl)
}

