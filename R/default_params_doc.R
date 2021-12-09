#' Documentation for main parameters of the model
#'
#' This function does nothing. It is intended to inherit is parameters'
#' documentation.
#'
#' @param comm a tibble with one row per individual in the community and three
#' columns:
#'
#'  * `z` contains (numeric) traits values
#'  * `species` contains species names (characters)
#'  * `ancestral_species` contains ancestral species names (characters)
#'  * (optionally, if immigration is enabled) `root_species` (characters), the
#'  mainland species that this species' clade originates from.
#'
#' @param island_comm a tibble with one row per individual in the community and three
#' columns:
#'
#'  * `z` contains (numeric) traits values
#'  * `species` contains species names (characters)
#'  * `ancestral_species` contains acnestral species names (characters)
#'  * (optionally, if immigration is enabled) `root_species` (characters), the
#'  mainland species that this species' clade originates from.
#'
#' @param mainland_comm a tibble defining what species exist in the mainland and
#' their trait distribution. One row per species, three columns:
#'
#'  * `species`, (character) the name of the species
#'  * `mean_z`, (numeric) mean value of the trait for that species
#'  * `z_sd`, (numeric) standard deviation of the trait for that species
#'
#' @param trait_ind numeric. Trait value `z` of a focal individual.
#' @param traits_comm numeric vector of variable length, the trait values of
#' every individual in the community.
#' @param mainland_nb_species integer, the number of species in the island
#' community.
#' @param mainland_z_sd standard deviation of the trait distribution of mainland
#' species
#' @param competition_sd numeric `>= 0`.Parameter \eqn{\sigma_{\alpha}} of
#' the competition coefficient. Modulates the the strength of competition
#' between two individuals given their distance in trait space.
#' @param trait_opt numeric. The optimal trait value, \eqn{z_{opt}}.
#' \code{get_carrying_cap(trait_opt) = carrying_cap_opt}.
#' @param carrying_cap_sd numeric `>= 0`. Parameter \eqn{\sigma_K} of the
#' carrying capacity. Modulates how fast the carrying capacity decays when
#' moving away from the optimal trait value.
#' @param carrying_cap_opt numeric, value of the carrying capacity at
#' `trait_opt`
#' @param growth_rate numeric `>= 0`, the baseline growth rate. Generations
#' being discrete, high values will cause chaos.
#' @param prob_mutation numeric between 0 and 1, the probability that any new
#' individual is sampled with a mutation.
#' @param mutation_sd numeric `>= 0`, the standard deviation of the normal
#' distrbution from which mutations are drawn.
#' @param trait_dist_sp numeric, the minimal trait distance between two
#' clusters of individuals triggering speciation.
#' @param nb_gens integer, how many generations should the simulation be run
#' for?
#' @param fitness_func, the name of the function used to compute fitness.
#' @param comrad_tbl a tibble containing the output of a `comrad` simulation,
#' as produced by [run_simulation()] or read by [read_comrad_tbl()].
#'
#' @author Theo Pannetier, based on skeleton stolen from Richel J.C. Bilderbeek.

default_params_doc <- function(
  comm,
  trait_ind,
  traits_comm,
  island_comm,
  mainland_comm,
  mainland_nb_species,
  mainland_z_sd,
  competition_sd,
  trait_opt,
  carrying_cap_opt,
  carrying_cap_sd,
  growth_rate,
  prob_mutation,
  mutation_sd,
  trait_dist_sp,
  nb_gens,
  fitness_func,
  comrad_tbl
) {
  # Nuffin
}
