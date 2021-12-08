#' Plot community size over generations
#'
#' Plots the number of individuals in the community over time.
#'
#' @param comrad_tbl a tibble containing the output of a `comrad` simulation,
#' as produced by [run_simulation()].
#' @param colouring character specifying how to colour each species. Default
#' `auto` lets `ggplot` pick colours as usual. `species_names` is used when
#' colour hexes are sampled as species names during the simulation, and will
#' colour each species with its colour. This ensures consistency across plots
#' but may make colours harder to distinguish based on which hexes where
#' sampled.
#' @param which_geom character specifying the geom to plot. `"area"` plots a
#' stack of area plots coloured by species,  `"line"` plots the population sizes
#' of each species in separate lines (still coloured by species).
#'
#' @author Théo Pannetier
#' @export

plot_comm_size <- function(comrad_tbl,
                           colouring = "species_names",
                           which_geom = "area") {
  comrad::test_comrad_comm(
    comrad_tbl %>% dplyr::select("z", "species", "ancestral_species")
  )
  if (!colouring %in% c("auto", "species_names")) {
    stop("'colouring' must be either 'auto' or 'species_names', see doc.")
  }
  if (!which_geom %in% c("area", "line")) {
    stop("'which_geom' must be either 'area' or 'line'.")
  }
  # Stupid but necessary for the build
  n <- NULL
  t <- NULL
  species <- NULL

  # Extract species names for colours
  species_names <- unique(comrad_tbl$species)
  names(species_names) <- species_names

  comrad_tbl$species <- factor(comrad_tbl$species, levels = species_names)

  growth_plot <- comrad_tbl %>%
    dplyr::group_by(t, species) %>%
    dplyr::count() %>%
    ggplot2::ggplot(ggplot2::aes(x = t, y = n)) +
    ggplot2::labs(x = "Generation", y = "Nb of individuals")
  if (which_geom == "line") {
    growth_plot <- growth_plot +
      ggplot2::geom_line(ggplot2::aes(colour = species), show.legend = FALSE)
  } else if (which_geom == "area") {
    growth_plot <- growth_plot +
      ggplot2::geom_area(ggplot2::aes(fill = species), show.legend = FALSE)
  }
  if (colouring == "species_names") {
    growth_plot <- growth_plot +
      ggplot2::scale_colour_manual(
        values = species_names,
        aesthetics = ifelse(which_geom == "line", "colour", "fill")
      )
  }

  growth_plot
}
