#' Set default sampling frequency
#'
#' Returns an appropriate sampling frequency based on the order of magnitude of
#' \code{nb_gens}.
#'
#' @inheritParams default_params_doc
#'
#' @details The sampling frequency is set as follows:
#' \itemize{
#'   \item if \code{nb_gens < 1000}, sampling frequency is set to 1
#'   (every generation).
#'   \item if \code{nb_gens >= 1000}, sampling frequency is set to twice
#'   the order of magnitude of \code{nb_gens} - 2. E.g. for
#'   \code{nb_gens >= 1000}, the community is sampled every 20
#'   generations, every 200 generations for \code{nb_gens >= 10000},
#'   etc.
#' }
#'
#' @author Theo Pannetier
#' @export

set_sampling_freq <- function(nb_gens) {
  comrad::testarg_num(nb_gens)
  comrad::testarg_pos(nb_gens)

  sampling_freq <- purrr::map_dbl(nb_gens, function(nb_gen) {
    max(1, 2 * 10 ^ floor(log10(nb_gen) - 2))
  })
  return(sampling_freq)
}
