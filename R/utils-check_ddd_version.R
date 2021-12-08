#' Check that the installed version of `DDD` allows for the functions
#' required for `comrad`
#'
#' imperfect, for now errors if the installed version of `DDD` is not satisfying.
#'
#' @author Theo Pannetier
#' @export
check_ddd_version <- function() {
  DDD::both_rates_vary(ddmodel = 12)
}
