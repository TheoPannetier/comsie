#' Sample a random `comrad` community
#'
#' Sample 100 individuals pooled in up to 5 random species, with
#' trait values sampled in a normal distribution. For testing purposes.
#'
#' @author Théo Pannetier

rand_comm <- function() {
  n <- sample(1:100, 1)
  nb_species <- sample(1:5, 1)
  name_pool <- paste0(
    charlatan::ch_taxonomic_genus(nb_species), "_",
    charlatan::ch_taxonomic_epithet(nb_species)
  )
  comm <- tibble::tibble(
    z = stats::rnorm(n, 0, sd = default_carrying_cap_sd()),
    species = name_pool[sample(1:nb_species, n, replace = TRUE)],
    ancestral_species = rep(as.character(NA), n)
  )
  comrad::test_comrad_comm(comm)
  comm
}
