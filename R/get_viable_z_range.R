#' Given K(z), return the min and max values of z viable for a population of some size
#'
#' @param pop_size integer, a population size
#' @inheritParams default_params_doc
#'
#' @export
#' @return double, the max value of z that can support a population of size `pop_size`
get_viable_z_range <- function(pop_size,
                             trait_opt,
                             carrying_cap_opt,
                             carrying_cap_sd
) {
  carr_cap_tbl <- tibble::tibble(
    "z" = seq(0, 20, 0.01),
    "k_z" = get_carrying_cap(
      trait_ind = z,
      trait_opt = trait_opt,
      carrying_cap_opt = carrying_cap_opt,
      carrying_cap_sd = carrying_cap_sd
    )
  )
  z_max <- max(carr_cap_tbl[carr_cap_tbl$k_z >= 100, ]$z)
  viable_z_range <- c(-z_max, z_max)
  return(viable_z_range)
}
