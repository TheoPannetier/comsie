#' Get the competition coefficients between an individual and the rest of the
#' community
#'
#' Returns the competition coefficients experienced by a focal individual from
#' each individual in the community, including the focal individual on itself.
#'
#' @inheritParams default_params_doc
#'
#' @return a numeric vector containing the competitive effect caused by each
#' individual in the community on the focal individual. Each effect varies
#' between 0 (no effect at all) and 1 (the two individuals have the same trait
#' value).
#'
#' @author Theo Pannetier
#' @export

get_comp_coeff_comm <- function(
  trait_ind,
  traits_comm,
  competition_sd = default_competition_sd()) {

  comrad::testarg_length(trait_ind, 1) # not vectorized!

  trait_dist <- (trait_ind - traits_comm) ^ 2

  coeffs <- exp(- (trait_dist / (2 * competition_sd ^ 2)))

  # Solve possible NaN issues --------------------------------------------------
  # NaNs can arise if both terms in the division are equal to 0 or Inf
  if (competition_sd == 0) {
    coeffs[which(trait_dist == 0)] <- 0 # as if trait_dist / competition_sd = 0
  }
  coeffs
}
