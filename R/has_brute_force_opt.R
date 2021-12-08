#' Are brute force options available?
#'
#' `comrad` has some "brute force" options (namely, using OpenMP and/or SIMD) to
#' speed up the simulations, but these break the build because of portability
#' issues. To preserve the build on `master`, these options are only enabled on
#' separate branch `brute_force`. This function returns whether brute force is
#' enabled on branch currently in use.
#'
#' @author Théo Pannetier
#' @export
#'
has_brute_force_opt <- function() {
  TRUE
}
