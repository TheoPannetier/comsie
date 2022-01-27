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
  immig_i <- sample(1:nrow(mainland_comm), n)

  # Increment nb immigration events
  mainland_comm$nb_immig_events[immig_i] <- mainland_comm$nb_immig_events[immig_i] + 1

  immigrant_pop <- tibble::tibble(
    "z" = stats::rnorm(
      n = n,
      mean = mainland_comm$mean_z[immig_i],
      sd = mainland_comm$sd_z[immig_i]
    ),
    "species" = mainland_comm$species[immig_i],
    "ancestral_species" = as.character(NA),
    "founder" = paste0(species, "_", mainland_comm$nb_immig_events[immig_i])
  )
  return(list("immigrant_pop" = immigrant_pop, "mainland_comm" = mainland_comm))
}
