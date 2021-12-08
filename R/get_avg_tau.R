#' Estimate the diversity-dependent trait evolution rate
#'
#' Estimate the average trait evolution rate over each state of
#' diversity for a `comrad` simulation.
#'
#' @inheritParams default_params_doc
#'
#' @author Theo Pannetier
#' @export
#'
get_avg_tau <- function(comrad_tbl) {

  # no NOTE
  # nolint start
  z <- NULL
  species <- NULL
  data <- NULL
  z_avg <- NULL
  d <- NULL
  tau <- NULL
  # nolint end

  # link time to corresponding diversity
  dtt <- comrad_tbl %>%
    dplyr::count(t, species) %>%
    dplyr::count(t, name = "d")

  # Compute average trait value per sp per timestep
  tau_tbl <- comrad_tbl %>%
    dplyr::group_by(species, t) %>%
    dplyr::summarise(
      "z_avg" = mean(z)
    ) %>%
    # Add diversity bins to the tbl
    dplyr::left_join(dtt, by = "t")

  # Split tbl by species
  tau_tbl <- tau_tbl %>%
    # Split by species (still grouped from above)
    tidyr::nest() %>%
    dplyr::mutate(
      data = purrr::map(data, function(df) {
        # Compute rate per species per timestep
        df %>% dplyr::mutate(
          "tau" = abs(z_avg - dplyr::lag(z_avg)) / (t - dplyr::lag(t))
        )
      })
    ) %>%
    # Pool back together
    tidyr::unnest(cols = c(data)) %>%
    dplyr::group_by(d) %>%
    # Average over time and species
    dplyr::summarise(
      "tau_avg" = mean(tau, na.rm = TRUE)
    )
  return(tau_tbl)
}
