#' Compute the delta nLTT between two trees
#'
#' Shortcut function calling [nLTT::nltt_diff_exact_extinct()]
#'
#' @param ltt_1 a `tibble` that contains the LTT of a tree, with columns
#' `time` and `N`, e.g. the output of [get_ltt_tbl()].
#' @param ltt_2 a `tibble` that contains the LTT of another tree, with columns
#' `time` and `N`, e.g. the output of [get_ltt_tbl()].
#'
#' @return a `double`, the delta nLTT between the two trees.
#' @export
#'
get_dnltt <- function(ltt_1, ltt_2) {
  dnltt <- nLTT::nltt_diff_exact_extinct(
    event_times = ltt_1$time,
    species_number = ltt_1$N,
    event_times2 = ltt_2$time,
    species_number2 = ltt_2$N,
    time_unit = "since"
  )
  return(dnltt)
}
