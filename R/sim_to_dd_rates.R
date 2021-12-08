#' Estimate speciation, extinction, and trait evo rates as function of diversity
#'
#' Computes estimates of the speciation, extinction and trait evolution rates
#' for each value of species diversity.
#'
#' @param comrad_tbls a list containing the output of a set of simulations,
#' as read by [read_comrad_tbl()]
#'
#' @author Theo Pannetier
#' @export
#'
sim_to_dd_rates <- function (comrad_tbls) {

  # no NOTE
  # nolint start
  N <- NULL
  d <- NULL
  tau_avg <- NULL
  # nolint end

  # Assert input
  purrr::walk(comrad_tbls, test_comrad_tbl)

  # Make phylogenies from comrad tables
  multi_phylo <- purrr::map(
    comrad_tbls,
    sim_to_phylo,
    include_stem = TRUE
  )
  # Estimate speciation and extinction rates
  bd_rates_tbl <- estimate_dd_rates(
    multi_phylo = multi_phylo
    ) %>%
    dplyr::rename("d" = N)

  # Estimate trait evo rate
  tau_tbl <- purrr::map_dfr(
    comrad_tbls,
    get_avg_tau,
    .id = "replicate"
  ) %>%
    dplyr::group_by(d) %>%
    dplyr::summarise(
      "trait_evo_rate" = mean(tau_avg)
      )
  rates_tbl <- bd_rates_tbl %>%
    dplyr::left_join(tau_tbl, by = "d")

  return(rates_tbl)
}
