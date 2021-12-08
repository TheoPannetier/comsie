#' Fit a diversity-dependent diversification model to phylogenies with fossil lineages
#'
#' Fits the parameters of a specified diversity-dependent model to  a set of
#' full phylogenies (with fossil lineages) produced by `comrad` simulations
#'
#' @param waiting_times_tbl a table with waiting times, the output of
#' [waiting_times()] for one or more phylogenies.
#' @param init_params named vector, initial values of the parameters to optimise.
#' The names and number of parameters must match those specified in
#' `dd_model$params_check`.
#' @param dd_model a list with five named elements that together specify the
#' diversity-dependent model:
#' * `name` a two-letter code, the name of the model. First letter specifies the
#' form of the speciation function, second letter the form of the extinction
#' function: "l" for "linear", "x" for exponential, "c" for constant.
#' * `speciation_func`, a function specifying the diversity-dependent speciation
#' rate. Must take arguments `params` and `N`.
#' * `extinction_func`, a function specifying the diversity-dependent extinction
#' rate. Must take arguments `params` and `N`.
#' * `constraints` a list of conditions that parameter values must satisfy. Each
#' element is a function that takes arguments `params` and `...`, and returns
#' `TRUE` if the constraint is satisfied, `FALSE` if it isn't.
#' * `params_check` a function that controls the format of `params`. Returns an
#' error if the elements of `params` are named differently from what is expected
#' or if the length differs from the expectation.
#'
#' `comrad` contains several `dd_model` functions, see for example
#' [comrad::dd_model_lc()].
#'
#' @param num_cycles passed to [DDD::optimizer()], number of cycles of
#' optimisation. Next cycle starts from the last vertices of the previous cycle.
#'
#' @return a one-row table with initial values of the parameters,
#' maximum likelihood estimates, the maximum likelihood and a convergence code
#' (see [subplex::subplex()] for the meaning of this code)
#'
#' @author Theo Pannetier
#' @export
#'
fit_dd_model_with_fossil <- function(waiting_times_tbl,
                                     init_params,
                                     dd_model = dd_model_lc(),
                                     num_cycles = Inf
) {

  # Format initial parameter values for output
  init_tbl <- init_params %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      "params" = names(init_params)
    ) %>%
    tidyr::pivot_wider(
      names_from = params,
      names_prefix = "init_",
      values_from = value
    )

  N_max <- max(waiting_times_tbl$N)

  # Unwrap DD model
  speciation_func <- dd_model$speciation_func
  extinction_func <- dd_model$extinction_func
  constraints <- dd_model$constraints

  are_constraints_ok <- function(constraints, params, ...) {
    constraints %>%
      purrr::map_lgl(
        function(func, params, ...) {
          func(params, ...)
        },
        params,
        N_max
      ) %>%
      all()
  }

  # Check initial parameters
  init_params %>% dd_model$params_check()
  if (!are_constraints_ok(constraints, init_params, N_max)) {
    warning("The constraints of the model are not satisfied for the initial parameter values.")
    loglik_tbl <- init_params %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        "params" = names(init_params),
        "value" = as.numeric(NA),
        "loglik" = as.numeric(NA),
        "conv" = as.numeric(NA),
        "num_cycles" = as.numeric(NA)
      ) %>%
      tidyr::pivot_wider(
        names_from = params,
        names_prefix = "ml_",
        values_from = value
      ) %>%
      dplyr::bind_cols(
        "dd_model" = dd_model$name,
        "with_fossil" = TRUE,
        init_tbl, .
      )
    return(loglik_tbl)
  }

  # Transform parameters
  init_trparsopt <- init_params %>% transform_pars()

  # Declare function to be optimised
  fun <- function(trparsopt) {
    if (min(trparsopt) < 0 || max(trparsopt) > 1) return(-Inf)
    params <- untransform_pars(trparsopt)
    if (!are_constraints_ok(constraints, params, N_max)) return(-Inf)
    loglik <- comrad::dd_loglik_func(
      waiting_times_tbl = waiting_times_tbl,
      params = params,
      speciation_func = speciation_func,
      extinction_func = extinction_func
    )
    return(loglik)
  }

  # Run maximum likelihood optimisation
  ml_output <- ddd_optimizer(
    fun = fun,
    trparsopt = init_trparsopt,
    num_cycles = num_cycles
  )

  # Format output
  loglik_tbl <- ml_output %>%
    tibble::as_tibble() %>%
    dplyr::select(par) %>%
    dplyr::mutate(
      "par" = par %>% untransform_pars(),
      "params" = names(par),
      "loglik" = ml_output$fvalues,
      "conv" = ml_output$conv,
      "num_cycles" = num_cycles
    ) %>%
    tidyr::pivot_wider(
      names_from = params,
      names_prefix = "ml_",
      values_from = par
    ) %>%
    dplyr::bind_cols(
      "dd_model" = dd_model$name,
      "with_fossil" = TRUE,
      init_tbl, .
    )
  return(loglik_tbl)
}
