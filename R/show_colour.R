show_colour <- function(col) {
  tibble::tibble(
    x = 1,
    y = 1
  ) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(colour = col, size = 50) +
    ggplot2::theme_void()
}
