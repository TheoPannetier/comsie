#' Exclude ephemeral species
#'
#' Exclude species sampled for less than a specified number of generations.
#'
#' @details Mind that generations not sampled in the table can of course not be
#' accounted for.
#'
#' @param comrad_tbl a tibble containing the output of a `comrad` simulation,
#' as produced by [run_simulation()].
#' @param min_nb_gens integer. Species that occur in less sampled generations
#' than this will be excluded.
#'
#' @author Théo Pannetier
#' @export

exclude_ephemeral_spp <- function(comrad_tbl,
                                  min_nb_gens = 2
) {
  comrad::test_comrad_tbl(comrad_tbl)
  comrad::testarg_num(min_nb_gens)
  comrad::testarg_int(min_nb_gens)

  species <- NULL # no NOTE
  n <- NULL # no NOTE

  ephemeral_spp <- comrad_tbl %>%
    dplyr::group_by(t, species) %>%
    dplyr::count() %>%
    dplyr::ungroup(t) %>%
    dplyr::count() %>%
    dplyr::arrange(n) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n < min_nb_gens) %>%
    dplyr::pull(species)

  comrad_tbl %>%
    dplyr::filter(!(species %in% ephemeral_spp))
}
