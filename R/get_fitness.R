#' Compute fitness values
#'
#' Fitness values are computed for each individual from their trait value,
#' the fitness landscape defined by the carrying capacity parameters, and the
#' trait values of all other individuals in the community.
#'
#' @inheritParams default_params_doc
#' @param brute_force_opt a string specifying which brute force option to use
#' to speed up the calculation of competition coefficients. Defaults to "none".
#' Other options are "omp", for multithreading with OpenMP, "simd" for single
#' instruction, multiple data (SIMD) via the C++ library
#' [`xsimd`](https://github.com/xtensor-stack/xsimd); and "simd_omp" for both.

#'
#' @author Theo Pannetier
#' @export

get_fitness <- function(
  traits_comm,
  growth_rate = default_growth_rate(),
  competition_sd = default_competition_sd(),
  trait_opt = default_trait_opt(),
  carrying_cap_opt = default_carrying_cap_opt(),
  carrying_cap_sd = default_carrying_cap_sd(),
  fitness_func = fitness_func_ricker,
  brute_force_opt = "none") {

  # Test argument type ---------------------------------------------------------
  comrad::testarg_num(growth_rate)
  comrad::testarg_pos(growth_rate)
  comrad::testarg_num(competition_sd)
  comrad::testarg_pos(competition_sd)
  comrad::testarg_num(trait_opt)
  comrad::testarg_num(carrying_cap_opt)
  comrad::testarg_pos(carrying_cap_opt)
  comrad::testarg_num(carrying_cap_sd)
  comrad::testarg_pos(carrying_cap_sd)

  # Compute effective population sizes -----------------------------------------
  n_eff <- comrad::get_n_eff_cpp(
    z = traits_comm,
    competition_sd = competition_sd,
    brute_force_opt = brute_force_opt
  ) # get the n_eff values experienced by each individual in the community

  # Compute k the carrying capacity --------------------------------------------
  carrying_cap <- comrad::get_carrying_cap(
    trait_ind = traits_comm,
    trait_opt = trait_opt,
    carrying_cap_opt = carrying_cap_opt,
    carrying_cap_sd = carrying_cap_sd
  )

  # Compute the fitness based on the Ricker model-------------------------------
  fitness <- fitness_func(
    growth_rate = growth_rate,
    n_eff = n_eff,
    carrying_cap = carrying_cap
  )
  comrad::testarg_num(fitness)
  comrad::testarg_length(fitness, length(traits_comm))

  return(fitness)
}
