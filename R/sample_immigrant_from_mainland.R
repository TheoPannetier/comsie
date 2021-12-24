#' Sample a number of individuals from a mainland community
#'
#' Individuals trait values are sampled in a normal distribution using the
#' parameters associated to the corresponding mainland species.
#'
#' @inheritParams default_params_doc
#' @param n integer, number of individuals to sample
#'
#' @export
sample_immigrant_from_mainland <- function(mainland_comm, n = 1) {

  source_pop <- dplyr::slice_sample(mainland_comm, n = 1)

  immigrant_pop <- tibble::tibble(
    "z" = stats::rnorm(
      n = n,
      mean = source_pop$mean_z,
      sd = source_pop$sd_z
    ),
    "species" = source_pop$species,
    "ancestral_species" = as.character(NA),
    "root_species" = species
  )
  return(immigrant_pop)
}
