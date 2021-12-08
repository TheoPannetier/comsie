#' Interpolate LTT for a given time sequence
#'
#' Given a lineage-through-time table and a time sequence, this function evaluates the value
#' of N at every time point in the sequence.
#'
#' @param ltt_tbl a `tibble` with columns `time` and `N`, e.g. the output of
#' [get_ltt_tbl()].
#' @param t_seq numeric vector, a sequence of time points.
#'
#' @return a `tibble` with columns `time` and `N`, where `time` corresponds to every time in `t_seq`
#' @author Theo Pannetier
#' @export
interpolate_ltt <- function(ltt_tbl, t_seq) {
  ltt_func <- ltt_tbl_to_func(ltt_tbl)
  ltt_tbl2 <- tibble::tibble("time" = t_seq, "N" = ltt_func(t_seq))
  return(ltt_tbl2)
}
