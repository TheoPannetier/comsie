#' Draw a new community from the current one
#'
#' From the trait values of the current community, compute the fitness, draw
#' offspring, apply mutations and resolve speciation events.
#'
#' @inheritParams default_params_doc
#' @param brute_force_opt a string specifying which brute force option to use
#' to speed up the calculation of competition coefficients. Defaults to "none".
#' Other options are "omp", for multithreading with OpenMP, "simd" for single
#' instruction, multiple data (SIMD) via the C++ library
#' [`xsimd`](https://github.com/xtensor-stack/xsimd); and "simd_omp" for both.
#'
#' @author Théo Pannetier
#' @export

draw_comm_next_gen <- function(
  island_comm,
  mainland_comm,
  growth_rate = comrad::default_growth_rate(),
  competition_sd = comrad::default_competition_sd(),
  trait_opt = comrad::default_trait_opt(),
  carrying_cap_opt = comrad::default_carrying_cap_opt(),
  carrying_cap_sd = comrad::default_carrying_cap_sd(),
  mutation_sd = comrad::default_mutation_sd(),
  trait_dist_sp = comrad::default_trait_dist_sp(),
  brute_force_opt = "none",
  used_species_names
) {
  # Test argument type ---------------------------------------------------------
  comsie::test_island_comm(island_comm)
  comrad::testarg_num(growth_rate)
  comrad::testarg_pos(growth_rate)
  comrad::testarg_num(competition_sd)
  comrad::testarg_pos(competition_sd)
  comrad::testarg_num(trait_opt)
  comrad::testarg_num(carrying_cap_opt)
  comrad::testarg_pos(carrying_cap_opt)
  comrad::testarg_num(carrying_cap_sd)
  comrad::testarg_pos(carrying_cap_sd)
  comrad::testarg_num(mutation_sd)
  comrad::testarg_pos(mutation_sd)

  # Compute fitness
  fitness_comm <- comrad::get_fitness(
    traits_comm = island_comm$z,
    growth_rate = growth_rate,
    competition_sd = competition_sd,
    trait_opt = trait_opt,
    carrying_cap_opt = carrying_cap_opt,
    carrying_cap_sd = carrying_cap_sd,
    brute_force_opt = brute_force_opt
  )
  if (any(!is.finite(fitness_comm))) {
    stop("One or more individuals had infinite fitness")
  }

  # Create next generation from parent fitness ---------------------------------
  nb_offspring_comm <- comrad::draw_nb_offspring(
    fitness = fitness_comm
  )
  comrad::testarg_length(nb_offspring_comm, length(island_comm$z))

  new_comm <- tibble::tibble(
    "z" = rep(island_comm$z, nb_offspring_comm),
    "species" = rep(island_comm$species, nb_offspring_comm),
    "ancestral_species" = rep(island_comm$ancestral_species, nb_offspring_comm),
    "founder" = rep(island_comm$founder, nb_offspring_comm)
    # new community inherits traits and species from parents
  )
  comrad::testarg_length(new_comm$z, sum(nb_offspring_comm))

  # Catch extinction -----------------------------------------------------------
  if (length(new_comm$species) < 1) {
    return(new_comm)
  }
  # Draw and apply mutations ---------------------------------------------------
  new_comm$z <- comrad::apply_mutations(
    traits_comm = new_comm$z,
    mutation_sd = mutation_sd
  )
  # Resolve speciation ---------------------------------------------------------
  new_comm <- comsie::apply_speciation(
    island_comm = new_comm,
    mainland_comm = mainland_comm,
    trait_dist_sp = trait_dist_sp,
    used_species_names = used_species_names
  )
  comsie::test_island_comm(new_comm)

  return(new_comm)
}
