#' Names of DD models implemented in `comrad`
#'
#' @export
dd_model_names <- function() {
  c("lc", "ll", "lx", "xc", "xl", "xx", "pc", "lp", "pl", "pp", "px", "xp")
}

#' Values of `DDD` argument `ddmodel` corresponding to `comrad` DD models
#'
#' @param dd_model_name character, the name of the DD model in comrad
#'
#' @return an integer code, value for this DD model in `DDD`
#' @author Theo Pannetier
#' @export
dd_model_comrad_to_ddd <- function(dd_model_name) {
  switch (dd_model_name,
    "lc" = 1,
    "pc" = 2,
    # "cl" = 3, # not used in comrad
    # "cp" = 4, # not used in comrad
    "ll" = 5,
    "lp" = 6,
    "pp" = 7,
    "pl" = 8,
    "xc" = 9,
    # "cx" = 10, # not used in comrad
    "lx" = 11,
    "xx" = 12,
    "xl" = 13,
    "xp" = 14,
    "px" = 15
  )
}

#' Colours associated with each DD model
#'
#' For consistent plotting
#'
#' @export
dd_model_colours <- function() {
  dd_colours <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",  "#66A61E", "#E6AB02", "#A6761D", "#666666", "#E41A1C", "#377EB8", "#5D99FD", "#FD5D99")
  names(dd_colours) <- dd_model_names()
  return(dd_colours)
}

#' List of all diversity-dependent models
#'
#' Shortcut function to call all DD function objects
#'
#' @export
dd_models <- function() {
  list(
    "lc" = dd_model_lc(),
    "ll" = dd_model_ll(),
    "lx" = dd_model_lx(),
    "xc" = dd_model_xc(),
    "xl" = dd_model_xl(),
    "xx" = dd_model_xx(),
    "pc" = dd_model_pc(),
    "lp" = dd_model_lp(),
    "pl" = dd_model_pl(),
    "pp" = dd_model_pp(),
    "px" = dd_model_px(),
    "xp" = dd_model_xp()
  )
}

#' Update power DD model names
#'
#'  Power DD functions used to be called "x2" ("alternative" exponential,
#'  in contrast to the true exponential functions referred to by "x). This
#'  function updates the names, useful if dealing with data produced by older
#'  versions of `comrad`.
#'
#'  @param x, a `dd_model` name
#'
#'  @return an updated `dd_model` name
#'
#'  @author Théo Pannetier
#'  @export
update_dd_model_name <- function(x) {
  dplyr::case_when(
    x == "xc2" ~ "pc",
    x == "xl2" ~ "pl",
    x == "lx2" ~ "lp",
    x == "xx2" ~ "pp",
    TRUE ~ x
  )
}

#' Plot a key to the colours used to represent DD models
#'
#' @export
plot_dd_model_colours <- function() {
  tibble::tibble(
    "dd_model" = dd_model_names(),
    "y" = 1
  ) %>%
    ggplot2::ggplot(ggplot2::aes(x = dd_model, y = y, fill = dd_model)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = dd_model_colours()) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    )
}

#' Find DD speciation function associated with a DD model
#'
#' @param dd_model character, one of [comrad::dd_model_names()]
#'
#' @return a character, "linear", "expo" or "power"
#' @export
dd_model_to_speciation_func <- function(dd_model) {
  dplyr::case_when(
    dd_model %in% c("lc", "ll", "lx", "lp") ~ "linear",
    dd_model %in% c("xc", "xl", "xx", "xp") ~ "expo",
    dd_model %in% c("pc", "pl", "pp", "px") ~ "power"
  )
}

#' Find DD extinction function associated with a DD model
#'
#' @param dd_model character, one of [comrad::dd_model_names()]
#'
#' @return a character, "constant", "linear", "expo" or "power"
#' @export
dd_model_to_extinction_func <- function(dd_model) {
  dplyr::case_when(
    dd_model %in% c("lc", "xc", "pc") ~ "constant",
    dd_model %in% c("ll", "xl", "pl") ~ "linear",
    dd_model %in% c("lx", "xx", "px") ~ "expo",
    dd_model %in% c("lp", "pp", "xp") ~ "power"
  )
}

#' Names of the DD functions
#'
#' @export
dd_func_names <- function() {
  c("constant", "linear", "expo", "power")
}

#' Colours associated with each DD function
#'
#' @export
dd_func_colours <- function() {
  c(
    "constant" = "#89ae8a",
    "linear" = "#E4C552",
    "expo" = "#398DCC",
    "power" = "#DE09D6"
  )
}

#' Plot a key to the colours used to represent DD functions
#'c
#' @export
plot_dd_func_colours <- function() {
  tibble::tibble(
    "dd_func" = dd_func_names(),
    "y" = 1
  ) %>%
    ggplot2::ggplot(ggplot2::aes(x = dd_func, y = y, fill = dd_func)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = dd_func_colours()) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    )
}
