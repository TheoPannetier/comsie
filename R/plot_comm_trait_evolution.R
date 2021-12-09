#' Plot the evolution of trait values in the community over generations
#'
#' Produces a hex-density plot of all trait values in the community over
#' generations.
#'
#' @param comrad_tbl a tibble containing the output of a `comrad` simulation,
#' as produced by [run_simulation()].
#' @param generation_range numeric vector with length 2, supplying the first and
#' last generation to plot from the dataset. Defaults to every generation in the
#' table
#' @param xgrain numeric, the width of  hexes on the x-axis (generations)
#' @param ygrain numeric, the width of  hexes on the y-axis (trait)
#' @param xlim numeric vector, the range of x-values, passed to
#' `ggplot::coord_cartesian()` if not `NULL`.
#' @param ylim numeric vector, the range of y-values, passed to
#' `ggplot::coord_cartesian()` if not `NULL`.
#' @param hex_fill character with two options, `"counts"` and `"species"`.
#' `"counts"` colours hexes by the count of individuals, and `"species"` by
#' species identity.
#' @param alpha transparency parameter passed to ggplot
#'
#' @author Théo Pannetier
#' @export


plot_comm_trait_evolution <- function(comrad_tbl,
                                      generation_range = c(0, Inf),
                                      xgrain = 200,
                                      ygrain = 0.01,
                                      xlim = NULL,
                                      ylim = NULL,
                                      alpha = 0.95,
                                      hex_fill = "species"
                                      ) {
  comrad::test_comrad_comm(
    comrad_tbl %>% dplyr::select(-t)
  )
  comrad::testarg_num(generation_range)
  comrad::testarg_pos(generation_range)
  comrad::testarg_length(generation_range, 2)
  comrad::testarg_num(xgrain)
  comrad::testarg_pos(xgrain)
  comrad::testarg_num(ygrain)
  comrad::testarg_pos(ygrain)

  if (!hex_fill %in% c("counts", "species", "clade")) {
    stop("'hex_fill' must be either 'counts' or 'species', or 'clade' see doc.")
  }
  if (!is.null(xlim[1]) && (!is.numeric(xlim) || length(xlim) != 2)) {
    stop("custom 'xlim' must be a length-2 numeric vector.")
  }
  if (!is.null(ylim[1]) && (!is.numeric(ylim) || length(ylim) != 2)) {
    stop("custom 'ylim' must be a length-2 numeric vector.")
  }

  # Stupid but necessary for the build
  z <- NULL
  t <- NULL
  species <- NULL

  if (generation_range[2] == Inf) {
    generation_range[2] <- max(comrad_tbl$t)
  }
  if (any(!(generation_range %in% comrad_tbl$t))) {
    stop(
      "generation_range is out of the scope of generations in the comrad_tbl."
    )
  }
  species_names <- unique(comrad_tbl$species)
  names(species_names) <- species_names

  trait_plot <- comrad_tbl %>%
    dplyr::filter(
      dplyr::between(t, generation_range[1], generation_range[2]),
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = t, y = z)) +
    ggplot2::scale_y_continuous(minor_breaks = seq(-10, 10, 0.1)) +
    ggplot2::labs(x = "Generation", y = "Trait") +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)

  if (hex_fill == "species") {
    trait_plot <- trait_plot +
      ggplot2::geom_hex(
        ggplot2::aes(fill = species, alpha = alpha),
        binwidth = c(xgrain, ygrain),
        show.legend = FALSE
      ) +
      ggplot2::scale_fill_manual(
        values = species_names,
      )
  } else if (hex_fill == "clade") {
    trait_plot <- trait_plot +
      ggplot2::geom_hex(
        ggplot2::aes(fill = root_species, alpha = alpha),
        binwidth = c(xgrain, ygrain),
        show.legend = FALSE
      ) +
      ggplot2::scale_fill_manual(
        values = species_names,
      )
  } else {
    trait_plot <- trait_plot +
      ggplot2::geom_hex(
        binwidth = c(xgrain, ygrain),
        show.legend = FALSE
      ) +
      viridis::scale_fill_viridis(option = "B")
  }
  trait_plot
}
