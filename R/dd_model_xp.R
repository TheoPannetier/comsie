#' Diversity-dependent model with exponential (x) dependence on speciation and
#' power (p) dependence on extinction
#'
#' A list specifying a DD model with exponential diversity-dependence on the
#' speciation rate and power diveristy-dependence on the extinction rate; to be
#' fed as argument `dd_model` to [comrad::fit_dd_model_with_fossil()].
#'
#'\deqn{\lambda(N) = \lambda_{0}(\alpha + (1 - \alpha) \frac{\mu_{0}}{\lambda_{0}})^{\frac{N}{K}}}
#'\deqn{\mu(N) = \mu_{0} \times N^{\frac{log\Big(\alpha\frac{\lambda_0 - \mu_0}{\lambda_0}+1\Big)}{log(K)}}}
#'
#' @author Theo Pannetier
#' @export
dd_model_xp <- function() {
  list(
    "name" = "xp",
    "speciation_func" = function(params, N) {
      params["lambda_0"] * (params["alpha"] + (1 - params["alpha"]) * (params["mu_0"] / params["lambda_0"])) ^ (N / params["k"])
    },
    "extinction_func" = function(params, N) {
      x <- log(1 + params["alpha"] * (params["lambda_0"] - params["mu_0"]) / params["mu_0"]) / log(params["k"])
      params["mu_0"] * (N ^ x)
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
        stop("params for ddmodel_xp should be \"lambda_0\", \"mu_0\", \"k\" and \"alpha\".")
      }
    },
    "DDD_name" = 14
  )
}
