#' Plot trait distribution through time
#'
#' Updated version of [comrad::plot_comm_traits_evolution()].
#' Main difference is the (faster) use of rectangles instead of hexes to
#' represent occupied areas of the trait distribution, and more straightforwards
#' plotting options.
#'
#' @param comsie_tbl a tibble, e.g. output of [read_comsie_tbl()]
#' @param fill_by How should rectangles be coloured? either "species" or "clade".
#' @param xlim length 2 num vector to pass to ggplot's `coord_cartesian()`
#' @param ylim length 2 num vector to pass to ggplot's `coord_cartesian()`
#' @param binwidths length 2 num vector, horizontal and vertical widths of the
#' rectangles.
#' @param alpha the ggplot parameter
#'
#' @export
plot_zt <- function(comsie_tbl, fill_by = "clade", xlim = c(0, max(comsie_tbl$t)), ylim = c(-7.5, 7.5), binwidths = c(100, 0.01), alpha = 1) {

  if (fill_by == "species") {
    comsie_tbl <- comsie_tbl %>% dplyr::rename("fill_var" = species)
  } else if (fill_by == "clade"){
    comsie_tbl <- comsie_tbl %>% dplyr::rename("fill_var" = founder)
  } else {
    stop("\"fill_by\" must be either species or clade.")
  }
  colour_names <- substr(unique(comsie_tbl$fill_var), 1, 7)
  names(colour_names) <- unique(comsie_tbl$fill_var)

  gg <- comsie_tbl %>%
    ggplot2::ggplot(ggplot2::aes(x = t, y = z)) +
    ggplot2::scale_y_continuous(minor_breaks = seq(-10, 10, 0.1)) +
    ggplot2::labs(x = "Generation", y = "Trait") +
    ggplot2::geom_bin2d(
      ggplot2::aes(fill = fill_var),
      alpha = alpha,
      binwidth = binwidths,
      size = 0.01,
      color = "black",
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = colour_names
    ) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::theme_bw()

  return(gg)
}
