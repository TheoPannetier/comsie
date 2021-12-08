#' Draw a set of initial parameter values for a DD model
#'
#' @param nb_sets integer, the number of sets desired.
#' @param phylos the list of `phylo` class objects on which the DD model is going
#'  to be fit.
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
#' @return a list of length-4 vectors containing initial values for `lambda_0`,
#' `mu_0` and `K` and `alpha`
#'
#' @author Theo Pannetier
#' @export
draw_init_params_dd_ml <- function(nb_sets, phylos, dd_model) {

  ltt_tbl <- comrad::get_ltt_tbl(phylos[[1]]) %>%
    dplyr::mutate("time" = time - min(time))
  n_max <- phylos %>%
    purrr::map(ape::drop.fossil) %>%
    purrr::map_int(ape::Ntip) %>%
    max()
  t_max <- max(ltt_tbl$time)
  proto_lambda0 <- log(n_max) / t_max

  # Initial parameter values
  lambdas <- stats::runif(nb_sets, proto_lambda0 * 0.5, proto_lambda0 * 2)
  mus <- stats::runif(nb_sets, 0, 0.75 * lambdas)
  ks <- trunc(n_max + n_max * stats::rgamma(nb_sets, shape = 0.5, scale = 0.3))
  alphas <- stats::runif(nb_sets, 0, 1)

  init_params <- purrr::pmap(
    list(lambdas, mus, ks, alphas),
    function(lambda, mu, k, alpha) {
    c("lambda_0" = lambda, "mu_0" = mu, "k" = k, "alpha" = alpha)
  })

  with_alpha <- !stringr::str_detect(dd_model$name, "c") # no alpha if constant

  if (!with_alpha) {
    init_params <- init_params %>% purrr::map(function(vec) vec[-4])
  }
  return(init_params)
}
