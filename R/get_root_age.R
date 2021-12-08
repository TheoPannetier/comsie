#' Get the age of the root of a tree
#'
#' A bit hacky; not the most efficient function but that's what I could come up with quickly.
#'
#' @param phylo a `phylo` class object
#' @param present_time the time at present, assumed to be 0 by default
#'
#' @return the root age, a double.
#'
#' @author Theo Pannetier
#' @export
get_root_age <- function(phylo, present_time = 0) {
  root_time <- get_ltt_tbl(phylo)$time[1]
  root_age <- present_time - root_time
  return(root_age)
}
