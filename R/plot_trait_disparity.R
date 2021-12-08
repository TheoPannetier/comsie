#' Plot trait disparity over time
#'
#' Compute and plot the variance in trait values in the community at every time
#' step
#'
#' @inheritParams default_params_doc
#' @param generation_range numeric vector with length 2, supplying the first and
#' last generation to plot from the dataset. Defaults to every generation in the
#' table
#'
#' @author Théo Pannetier
#' @export

plot_trait_disparity <- function(comrad_tbl,
                                 generation_range = c(0, Inf)
) {
  comrad::test_comrad_comm(
    comrad_tbl %>% dplyr::select("z", "species", "ancestral_species")
  )
  comrad::testarg_num(generation_range)
  comrad::testarg_pos(generation_range)
  comrad::testarg_length(generation_range, 2)

  z <- NULL
  trait_disparity <- NULL

  comrad_tbl %>%
    dplyr::group_by(t) %>%
    dplyr::summarise(
      trait_disparity = stats::var(z)
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = t, y = trait_disparity)) +
    ggplot2::geom_area() +
    ggplot2::labs(
      x = "Generations",
      y = bquote(sigma(z) ^ 2)
    )

}
