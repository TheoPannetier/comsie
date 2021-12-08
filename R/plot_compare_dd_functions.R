#' Quickly compare the three diversity-dependent functions
#'
#' Draws a quick plot of linear, exponential and power speciation and extinction
#' functions for the requested parameter values and range of N
#'
#' @param params a numeric vector with four named elements, `"alpha_0"`, `"mu_0`,
#' `"k"` and `"alpha"`.
#' @param N a numeric vector, range of values of N to plot
#'
#' @export
plot_compare_dd_functions <- function(params, N) {
  rates_tbl <- bind_rows(
    rates_from_dd_model(
      N = N,
      dd_model = dd_model_ll(),
      params = params
    ),
    rates_from_dd_model(
      N = N,
      dd_model = dd_model_xx(),
      params = params
    ),
    rates_from_dd_model(
      N = N,
      dd_model = dd_model_pp(),
      params = params
    )
  )

  rates_tbl %>%
    dplyr::mutate(
      "rate" = as_factor(rate),
      "dd_model" = forcats::fct_recode(
        dd_model,
        "linear" = "ll",
        "power" = "pp",
        "exponential" = "xx"
      )
    ) %>%
    ggplot(aes(x = N, y = value, linetype = rate, colour = dd_model)) +
    geom_line() +
    theme_bw() +
    scale_colour_manual(values = c(
      "linear" = "#D95F02",
      "power" = "#377EB8",
      "exponential" = "#E6AB02"
    )) +
    labs(
      colour = "Function",
      linetype = "Rate",
      y = "Rate value"
    )
}
