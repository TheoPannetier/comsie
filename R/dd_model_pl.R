#' Diversity-dependent model with power (p) dependence on speciation and
#' linear (l) dependence on extinction
#'
#' A list specifying a DD model with power diversity-dependence on the
#' speciation rate and linear diversity-dependence extinction rate;
#' to be fed as argument `dd_model` to [comrad::fit_dd_model_with_fossil()].
#' This corresponds to the "exponential" model found in `DDD` (`ddmodel = 8`)
#'
#'\deqn{\lambda(N) = \lambda_{0}(\alpha + (1 - \alpha) \frac{\mu_{0}}{\lambda_{0}})^{\frac{N}{K}}}
#'\deqn{\mu(N) = \mu_{0} + \alpha(\lambda_{0} - \mu_{0}) \frac{N}{K}}
#'
#' @author Theo Pannetier
#' @export
dd_model_pl <- function() {
  list(
    "name" = "pl",
    "speciation_func" = function(params, N) {
      x <- log(params["lambda_0"] / (params["alpha"] * (params["lambda_0"] - params["mu_0"]) + params["mu_0"])) / log(params["k"])
      params["lambda_0"] * N ^ (-x)
    },
    "extinction_func" = function(params, N) {
      params["mu_0"] + params["alpha"] * (params["lambda_0"] - params["mu_0"]) * (N / params["k"])
    },
    "constraints" = list(
      function(params, ...) params["lambda_0"] > 0,
      function(params, ...) params["mu_0"] >= 0,
      function(params, ...) params["k"] >= 1,
      function(params, ...) params["lambda_0"] > params["mu_0"],
      function(params, ...) params["alpha"] >= 0 & params["alpha"] <= 1
    ),
    "params_check" = function(params) {
      params_names <- names(params)
      if (!(length(params_names) == 4 &&
            all(params_names %in% c("lambda_0", "mu_0", "k", "alpha"))
      )) {
        stop("params for ddmodel_pl should be \"lambda_0\", \"mu_0\", \"k\" and \"alpha\".")
      }
    },
    "DDD_name" = 8
  )
}
