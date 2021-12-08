#' Plot the fitness landscape at a given time in the simulation
#'
#' Computes the fitness landscape along trait range `z_seq` at time `generation`
#' in the simulation
#'
#' @inheritParams default_params_doc
#' @param generation numeric, a single generation step in the simulation.
#' @param z_seq numeric vector, the sequence of trait values along which the
#' fitness landscape should be computed.
#' Default `z_seq = NULL` sets the sequence to every `0.01` between twice the
#' minimal and maximal value in `comrad_tbl`
#'
#' @author Théo Pannetier
#' @export

plot_fitness_landscape <- function(comrad_tbl,
                                   generation,
                                   z_seq = NULL,
                                   competition_sd,
                                   carrying_cap_sd) {
  z <- NULL
  fitness <- NULL

  comrad::test_comrad_comm(
    comrad_tbl %>% dplyr::select("z", "species", "ancestral_species")
  )
  comrad::testarg_num(generation)
  comrad::testarg_pos(generation)
  comrad::testarg_num(competition_sd)
  comrad::testarg_pos(competition_sd)
  comrad::testarg_num(carrying_cap_sd)
  comrad::testarg_pos(carrying_cap_sd)

  if (!generation %in% comrad_tbl$t) {
    stop(paste("Generation", generation, "wasn't sampled."))
  }

  max_gen <- max(comrad_tbl$t)

  if (is.null(z_seq)) {
    z_range <- comrad_tbl$z[comrad_tbl$t == generation]
    z_seq <- seq(min(z_range) * 2.0, max(z_range) * 2.0, by = 0.01)
  }

  traits_comm <- comrad_tbl %>%
    dplyr::filter(t == generation) %>%
    dplyr::select(z) %>%
    unlist()

  fitness_landscape <- get_fitness_landscape(
    z_seq = z_seq,
    traits_comm = traits_comm,
    growth_rate = default_growth_rate(),
    competition_sd = competition_sd,
    carrying_cap_opt = default_carrying_cap_opt(),
    carrying_cap_sd = carrying_cap_sd
  )

  fitness_landscape %>%
    ggplot2::ggplot(ggplot2::aes(x = z, y = fitness)) +
    ggplot2::geom_area() +
    ggplot2::labs(
      title = paste("Generation", generation, "/", max_gen)
    )
}
