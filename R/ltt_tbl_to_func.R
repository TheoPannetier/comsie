#' Transform the LTT table into a step function
#'
#' Converts the lineage-through-time table of a tree into a stepwise function of
#' time with [stats::stepfun()]
#'
#' @param ltt_tbl a `tibble` with columns `time` and `N`, e.g. the output of
#' [get_ltt_tbl()].
#' @return a stepwise function of time that returns the number of lineages
#' in the tree at any point in time
#' @author Théo Pannetier
#' @export
#'
ltt_tbl_to_func <- function(ltt_tbl) {
  if (nrow(ltt_tbl) > 1) {
    ltt_func <- stats::stepfun(
      x = ltt_tbl$time[-1],
      y = ltt_tbl$N
    )
  } else {
    ltt_func <- function(t) ltt_tbl$time
  }
  return(ltt_func)
}
