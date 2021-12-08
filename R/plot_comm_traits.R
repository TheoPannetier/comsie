#' Plot trait distribution in a generation
#'
#' Plots a histogram of the distribution of trait values in a single generation.
#'
#' @param comrad_tbl a tibble containing the output of a `comrad` simulation,
#' as produced by [run_simulation()].
#' @param generation numeric, the index of the generation to plot
#' @param binwidth numeric, the width used for binning.
#'
#' @author Théo Pannetier
#' @export

plot_comm_traits <- function(comrad_tbl,
                             generation,
                             binwidth = 0.01) {
  comrad::test_comrad_comm(
    comrad_tbl %>% dplyr::select("z", "species", "ancestral_species")
  )
  comrad::testarg_num(generation)
  comrad::testarg_pos(generation)
  comrad::testarg_num(binwidth)
  comrad::testarg_pos(binwidth)

  # Stupid but necessary for the build
  z <- NULL
  species <- NULL

  if (!generation %in% comrad_tbl$t) {
    stop(paste("Generation", generation, "wasn't sampled."))
  }

  max_gen <- max(comrad_tbl$t)

  # Extract species names for colours
  species_names <- unique(comrad_tbl$species)
  names(species_names) <- species_names

  gen_data <- comrad_tbl %>% dplyr::filter(comrad_tbl$t == generation)
  gen_data %>%
    ggplot2::ggplot(ggplot2::aes(x = z, fill = species)) +
    ggplot2::geom_histogram(
      binwidth = binwidth,
      alpha = 0.6,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = species_names
    ) +
    ggplot2::scale_x_continuous(
      minor_breaks = seq(-10, 10, 0.1)
    ) +
    ggplot2::labs(
      x = "Trait",
      y = "Count",
      title = paste("Generation", generation, "/", max_gen)
    )
}
