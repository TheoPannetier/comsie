#' Diversity-dependent model with power (p) dependence on speciation and
#' constant (c) extinction
#'
#' A list specifying a DD model with power diversity-dependence on the
#' speciation rate and constant-rate extinction; to be fed as argument
#' `dd_model` to [comrad::fit_dd_model_with_fossil()]. This corresponds to the "exponential" model found in `DDD` (`ddmodel = 2`)
#'
#' \deqn{\lambda(N) = \lambda_{0} \times N^{-\frac{log(\frac{\lambda_{0}}{\mu_{0}})}{log(K)}} \\ \mu(N) = \mu_{0}}
#'
#' @author Theo Pannetier
#' @export
dd_model_pc <- function() {
  list(
    "name" = "pc",
    "speciation_func" = function (params, N) {
      params["lambda_0"] * N ^ (-log(params["lambda_0"] / params["mu_0"]) / log(params["k"]))
    },
    "extinction_func" = function (params, N) {
      rep(params["mu_0"], length(N))
    },
    "constraints" = list(
      function(params, ...) params["lambda_0"] > 0,
      function(params, ...) params["mu_0"] > 0,
      function(params, ...) params["k"] >= 1,
      function(params, ...) params["lambda_0"] > params["mu_0"]
    ),
    "params_check" = function(params) {
      params_names <- names(params)
      if (!(length(params_names) == 3 &&
            all(params_names %in% c("lambda_0", "mu_0", "k"))
      )) {
        stop("params for ddmodel_pc should be \"lambda_0\", \"mu_0\" and \"k\".")
      }
    },
    "DDD_name" = 2
  )
}

