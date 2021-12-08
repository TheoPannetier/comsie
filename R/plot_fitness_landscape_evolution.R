#' Plot the fitness landscape in a `comrad` simulation over time
#'
#' Computes the fitness landscape along the time sequence defined by
#' `generation_range` in a 3D plot.
#'
#' @inheritParams default_params_doc
#' @param z_seq numeric vector, the sequence of trait values along which the
#' fitness landscape should be computed.
#' Default `z_seq = NULL` sets the sequence to every `0.01` between twice the
#' minimal and maximal value in `comrad_tbl`.
#' @param generation_range numeric vector with length 2, supplying the first and
#' last generation to plot from the dataset. Defaults to every generation in the
#' table
#' @param fitness_lim numeric vector of length 2, the plotting range for the
#' z-axis, the fitness.
#'
#' @author Théo Pannetier
#' @export
#'
plot_fitness_landscape_evol <- function(comrad_tbl,
                                             z_seq = NULL,
                                             generation_range = c(0, Inf),
                                             fitness_lim = c(0, get_fitness(0)),
                                             competition_sd,
                                             carrying_cap_sd
) {
  comrad::test_comrad_comm(
    comrad_tbl %>% dplyr::select("z", "species", "ancestral_species")
  )
  comrad::testarg_num(generation_range)
  comrad::testarg_pos(generation_range)
  comrad::testarg_length(generation_range, 2)
  comrad::testarg_num(fitness_lim)
  comrad::testarg_pos(fitness_lim)
  comrad::testarg_length(fitness_lim, 2)
  comrad::testarg_num(competition_sd)
  comrad::testarg_pos(competition_sd)
  comrad::testarg_num(carrying_cap_sd)
  comrad::testarg_pos(carrying_cap_sd)

  if (generation_range[2] == Inf) {
    generation_range[2] <- max(comrad_tbl$t)
  }
  if (any(!(generation_range %in% comrad_tbl$t))) {
    stop(
      "generation_range is out of the scope of generations in the comrad_tbl."
    )
  }
  comrad_tbl <- comrad_tbl %>%
    dplyr::filter(
      dplyr::between(t, generation_range[1], generation_range[2]),
    )

  if (is.null(z_seq)) {
    z_seq <- seq(min(comrad_tbl$z) * 2.0, max(comrad_tbl$z) * 2.0, by = 0.01)
  }
  t_seq <- unique(comrad_tbl$t)

  landscape_seq <- lapply(
    t_seq,
    function(t) {
      traits_comm <- comrad_tbl$z[comrad_tbl$t == t]
      get_fitness_landscape(
        z_seq = z_seq,
        traits_comm = traits_comm,
        competition_sd = competition_sd,
        carrying_cap_sd = carrying_cap_sd
      ) %>%
        cbind(t)
    })
  landscape_seq <- do.call("rbind", landscape_seq)

  lattice::wireframe(
    fitness ~ t * z,
    data = landscape_seq,
    scales = list(arrows = F),
    shade = TRUE,
    ylim = c(0, max(landscape_seq$z))
  )
}
