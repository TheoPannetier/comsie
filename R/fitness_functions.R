#' Fitness functions
#'
#' A set of equations to compute the fitness of an individual. All three
#' functions specify a decline of fitness with local niche saturation, but
#' the starting and saturation fitness differ (see Details).
#'
#' @inheritParams default_params_doc
#' @param n_eff Effective population size experienced by an individual, see
#' [get_n_eff()]
#' @param carrying_cap The carrying capacity experienced by an individual, see
#' [get_carrying_cap()].
#'
#' @details
#' The table below reports the fitness values associated with hallmark levels
#' of saturation. Note that all three functions are `pmax()`'d.
#' \tabular{cccccc}{
#'   \strong{Neff / K} \tab \strong{0} \tab \strong{1/2} \tab
#'   \strong{1} \tab \strong{2} \tab \strong{infty} \cr
#'   logistic \tab r \tab r/2 \tab 0 \tab 0 \tab 0 \cr
#'   pontarp \tab 1 + r \tab 1 + r/2 \tab 1 \tab 1 - r \tab 0 \cr
#'   ricker \tab exp(r) \tab exp(r/2) \tab 1 \tab exp(-r) \tab 0 \cr
#' }
#'
#' @author Théo Pannetier
#'
#' @name fitness_func
NULL

#' @rdname fitness_func
#' @export
fitness_func_logistic <- function(growth_rate,
                                  n_eff,
                                  carrying_cap) {
  # Compute fitness with positive logistic function
  fitness <- pmax(0, growth_rate * (1 - n_eff / carrying_cap))

  # Solve possible NaN issues --------------------------------------------------
  if (
    # In case of conflict between parameters
    (growth_rate == 0 && any((n_eff / carrying_cap) %in% c(Inf, -Inf))) ||
    (growth_rate == Inf && any((n_eff / carrying_cap) == 1))
  ) {
    # I rule that growth_rate has precedence
    nans <- which(is.nan(fitness))
    fitness[nans] <- pmax(0, growth_rate)
  }

  fitness
}

#' @rdname fitness_func
#' @export
fitness_func_pontarp <- function(growth_rate,
                                 n_eff,
                                 carrying_cap) {
  # Compute fitness with positive logistic function
  fitness <- pmax(0, 1 + growth_rate * (1 - n_eff / carrying_cap))

  # Solve possible NaN issues --------------------------------------------------
  if (
    # In case of conflict between parameters
    (growth_rate == 0 && any((n_eff / carrying_cap) %in% c(Inf, -Inf))) ||
    (growth_rate == Inf && any((n_eff / carrying_cap) == 1))
  ) {
    # I rule that growth_rate has precedence
    nans <- which(is.nan(fitness))
    fitness[nans] <- pmax(0, 1 + growth_rate)
  }

  fitness
}

#' @rdname fitness_func
#' @export
fitness_func_ricker <- function(growth_rate,
                                n_eff,
                                carrying_cap) {
  # Compute fitness with the function from the Ricker model
  fitness <- exp(growth_rate * (1 - n_eff / carrying_cap))

  # Solve possible NaN issues --------------------------------------------------
  if (
    # In case of conflict between parameters
    (growth_rate == 0 && any((n_eff / carrying_cap) %in% c(Inf, -Inf))) ||
    (growth_rate == Inf && any((n_eff / carrying_cap) == 1))
  ) {
    # I rule that growth_rate has precedence
    nans <- which(is.nan(fitness))
    fitness[nans] <- exp(growth_rate)
  }

  fitness
}
