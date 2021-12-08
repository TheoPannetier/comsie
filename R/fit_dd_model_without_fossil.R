#' Fit a diversity-dependent diversification model to phylogenies without fossil lineages
#'
#' Wrapper around [DDD::dd_ML()]. Fits the parameters of a specified
#' diversity-dependent model the reconstructed phylogeny of a single tree
#' produced by `comrad` simulations.
#'
#' @param branching_times a vector containing the branching times of a single, ultrametric
#' (all tips reach the present) phylogenetic tree. Tip: from the full `phylo`, do
#' `phylo %>% ape::drop.fossil() %>% ape::branching.times()`.
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
#' @param verbose logical, should the output of every optimisation iteration be
#' printed to console?
#'
#' @return a one-row table with the model name, initial values of the parameters,
#' maximum likelihood estimates and the maximum likelihood
#'
#' @author Theo Pannetier
#' @export
#'
fit_dd_model_without_fossil <- function(
  branching_times,
  init_params,
  dd_model = dd_model_lc(),
  num_cycles = Inf,
  methode = "odeint::runge_kutta_cash_karp54",
  verbose = FALSE
) {
  check_ddd_version()
  both_rates_vary <- !stringr::str_detect(dd_model$name, "c")
  N_max <- max(ceiling(1.8 * (length(branching_times) + 1)), 10) # arbitrary upper limit

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
  # Format initial parameters for DDD
  initparsopt <- init_params
  # alpha to r
  if (both_rates_vary) {
    initparsopt[4] <- initparsopt[4] / (1 - initparsopt[4])
  }
  # Run dd_ML
  ddd_ml <- try(DDD::dd_ML(
    brts = branching_times,
    initparsopt = initparsopt,
    ddmodel = dd_model_comrad_to_ddd(dd_model$name),
    methode = methode,
    optimmethod = "simplex",
    res = N_max,
    num_cycles = num_cycles,
    verbose = verbose
  ))

  if (!is.data.frame(ddd_ml)) { # default results in case of an error
    if (both_rates_vary) {
      ddd_ml <- tibble::tibble(lambda = NA, mu = NA, K = NA, r = NA, loglik = -Inf, df = -1, conv = -1)
    } else {
      ddd_ml <- tibble::tibble(lambda = NA, mu = NA, K = NA, loglik = -Inf, df = -1, conv = -1)
    }
  }

  ddd_ml <- ddd_ml %>%
   dplyr::select(-df) %>%
    dplyr::rename_with(tolower)

  if (both_rates_vary) {
    ml_params <- ddd_ml %>%
      dplyr::mutate("alpha" = ifelse(is.infinite(r), 1, r / (1 + r)), .before = r) %>%
      dplyr::select(-r) %>%
      dplyr::rename_with(~paste0("ml_", .x), lambda:alpha)
  } else {
    ml_params <- ddd_ml %>%
      dplyr::rename_with(~paste0("ml_", .x), lambda:k)
  }
  ml_tbl <- dplyr::bind_cols(
    "dd_model" = dd_model$name,
    "with_fossil" = FALSE,
    init_tbl,
    ml_params,
    "num_cycles" = num_cycles
  )
  return(ml_tbl)
}
