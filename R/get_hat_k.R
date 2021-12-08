#' Estimate equilibrium diversity from LTT table
#'
#' Estimate `hat_k`, an empircial measure of equilibrium diversity
#' at the end of the simulation.
#' `hat_k` is simply computed as the average number of species `N` over
#' replicate simulations and over the last sixth of the simulation time.
#'
#' @param avg_ltt_tbl a `tibble` with the average number of species across
#' replicate simulations, e.g. the output of [avg_ltt()]
#'
#' @export
get_hat_k <- function(avg_ltt_tbl) {
  t_seq <- avg_ltt_tbl$time

  avg_ltt_tbl %>%
    # Last 8th of the time sequence
    slice_tail(n = round(length(t_seq) / 8)) %>%
    pull(mean) %>%
    mean() %>%
    round(digits = 1)
}
