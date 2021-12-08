#' Create the mainland community
#'
#' Create a static pool of mainland species to draw immigrants from, with
#' the parameters of the associated trait distribution
#'
#' @details trait values of individuals will be sampled in a normal distribution,
#' with the mean of each species sampled in a uniform distribution bounded by
#' `z_range`.
#'
#' @inheritParams default_params_doc
#' @param z_range a range of trait values to sample the mean trait values of
#' each species from.
#'
#' @export
create_mainland_comm <- function(mainland_nb_species,
                                 z_range,
                                 mainland_z_sd = 0.2
                                 ){
  if (!length(mainland_z_sd) %in% c(1, mainland_nb_species)) {
    stop("There must be either one mainland_z_sd or as many as mainland_nb_species")
  }
  species_names <- NULL
  while (is.null(species_names) || any(duplicated(species_names))) {
    species_names <- charlatan::ch_hex_color(mainland_nb_species)
  }
  mean_zs <- stats::runif(mainland_nb_species, z_range[1], z_range[2])

  mainland_comm <- tibble::tibble(
    "species" = species_names,
    "mean_z" = mean_zs,
    "sd_z" = mainland_z_sd
  )
  return(mainland_comm)
}
