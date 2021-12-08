#' Plot an animation of trait distribution through generations
#'
#' Animated version of [plot_comm_traits()], lopping through generations. Dope.
#'
#' @param comrad_tbl a tibble containing the output of a `comrad` simulation,
#' as produced by [run_simulation()].
#' @param generation_range numeric vector with length 2, supplying the first and
#' last generation to plot from the dataset. Defaults to every generation in the
#' table
#' @param binwidth numeric, the width used for binning.
#'
#' @author Théo Pannetier
#' @export

plot_comm_traits_anim <- function(comrad_tbl,
                                  generation_range = c(0, Inf),
                                  binwidth = 0.01) {
  comrad::test_comrad_comm(
    comrad_tbl %>% dplyr::select("z", "species", "ancestral_species")
  )
  comrad::testarg_num(generation_range)
  comrad::testarg_pos(generation_range)
  comrad::testarg_length(generation_range, 2)
  comrad::testarg_num(binwidth)
  comrad::testarg_pos(binwidth)

  if (!"t" %in% colnames(comrad_tbl)) {
    stop("'comrad_tbl' must contain a column 't' with generation times.")
  }
  if (generation_range[2] == Inf) {
    generation_range[2] <- max(comrad_tbl$t)
  }
  if (any(!(generation_range %in% comrad_tbl$t))) {
    stop(
      "generation_range is out of the scope of generations in the comrad_tbl."
    )
  }

  # Stupid but necessary for the build
  z <- NULL
  species <- NULL

  max_gen <- max(comrad_tbl$t)

  # Extract species names for colours
  species_names <- unique(comrad_tbl$species)
  names(species_names) <- species_names

  zz <- comrad_tbl %>%
    dplyr::filter(
      dplyr::between(t, generation_range[1], generation_range[2]),
    ) %>%
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
      y = "Count"
    )
  zz + gganimate::transition_time(as.integer(t)) +
    ggplot2::labs(
      title = paste("Generation: {frame_time} /", max_gen)
    )

}
