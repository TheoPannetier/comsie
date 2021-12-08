#' Find gaps in trait values
#'
#' Runs through an ordered vector of trait values, returns the positions of gaps
#' `>= trait_dist_sp` between consecutive values.
#'
#' @param traits a numeric vector, trait values **in ascending order**.
#' @inheritParams default_params_doc
#'
#' @author Théo Pannetier
#' @export

find_trait_gaps <- function(traits, trait_dist_sp = default_trait_dist_sp()) {
  comrad::testarg_num(traits)
  if (any(traits != sort(traits))) {
    stop("'traits' must be sorted by ascending order before checking for gaps.")
  }
  trait_dist <- round(abs(diff(traits)), digits = 2)
  gaps <- which(trait_dist >= trait_dist_sp)
  return(gaps)
}
