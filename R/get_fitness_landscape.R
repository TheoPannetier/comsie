#' Compute the fitness along a range of trait values, given a community
#'
#' Computes the fitness values at every point in `z_seq`, given the community
#' defined by `traits_comm`. This is different from [get_fitness()],
#' where the fitness is only computed for the trait values of individuals in the
#' community.
#'
#' @inheritParams default_params_doc
#' @param z_seq numeric vector, the sequence of trait values along which the
#' fitness landscape should be computed.
#' @param traits_comm numeric vector, the trait values of all individuals in the
#' community for which the fitness landscape is to be computed
#'
#' @return a `tibble` with `z_seq` rows and columns `z`, `fitness`.
#'
#' @author Théo Pannetier
#' @export

get_fitness_landscape <- function(z_seq,
                                  traits_comm,
                                  growth_rate = default_growth_rate(),
                                  competition_sd = default_competition_sd(),
                                  carrying_cap_opt = default_carrying_cap_opt(),
                                  carrying_cap_sd = default_carrying_cap_sd()
) {
  # Compute intermediary variables
  k <- get_carrying_cap(
    z_seq,
    carrying_cap_opt = carrying_cap_opt,
    carrying_cap_sd = carrying_cap_sd
  )
  n_eff <-  get_n_eff(
    z_seq,
    traits_comm = traits_comm,
    competition_sd = competition_sd
  )
  # Compute fitness, wrap in tibble
  fitness_landscape <- tibble::tibble(
    "z" = z_seq,
    "fitness" = exp(growth_rate * (1 - n_eff / k))
  )
  fitness_landscape
}
