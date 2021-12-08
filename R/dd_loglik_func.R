#' Log-likelihood function of a diversity-dependent model given waiting times
#'
#' Compute the log-likelihood of athe diversity-dependent model specified by
#' `speciation_func`, `extinction_func`, and `params` given a set of waiting
#' times to next events and associated diversity.
#' Meant to be called internally by [fit_dd_model_with_fossil()].
#'
#' @param waiting_times_tbl the output of [waiting_times()], a table containing the
#' waiting times to a next event, type of events, and species diversity.
#' @param params a named vector containing the values of the parameters of
#' `speciation_func` and `extinction_func`.
#' @param speciation_func a function describing how the rate of speciation varies
#' with the number of species `N`. Must take arguments `params` and `N`. Inside
#' the function, parameters must be referred as `params$param_name`.
#' @param extinction_func a function describing how the rate of extinction varies
#' with the number of species `N`. Must take arguments `params` and `N`. Inside
#' the function, parameters must be referred as `params$param_name`.
#'
#'@return a unique log-likelihood value
#'
#'@author Theo Pannetier
#'@export
#'
dd_loglik_func <- function(params,
                           waiting_times_tbl,
                           speciation_func,
                           extinction_func) {

  wt <- waiting_times_tbl # shorter syntax
  wt$lambda_d <- speciation_func(params = params, N = wt$N)
  wt$mu_d <- extinction_func(params = params, N = wt$N)

  wt$log_lambda <- wt$is_speciation * log(wt$lambda_d)
  # if NaN (not speciation and lambda_d=0), should still be 0 bc not speciation
  wt$log_lambda[is.nan(wt$log_lambda)] <- 0

  wt$log_mu <- (!wt$is_speciation) * log(wt$mu_d)
  # if NaN (not extinction and mu_d=0), should still be 0 bc not extinction
  wt$log_mu[is.nan(wt$log_mu)] <- 0

  wt$probs <- log(wt$N) + wt$log_lambda + wt$log_mu -
    (wt$lambda_d + wt$mu_d) * wt$N * wt$waiting_time

  loglik <- sum(wt$probs)

  return(loglik)
}
