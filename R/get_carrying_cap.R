#' Get the carrying capacity for a given trait value
#'
#' Computes the carrying capacity experienced by an individual.
#'
#' @inheritParams default_params_doc
#'
#' @details the carrying capacity controls the static component of the fitness,
#' one that depends only on an individual's trait and not on the
#' presence/absence of competitors. It defines the fitness landscape before any
#' competitive effect.
#'
#' @author Theo Pannetier
#' @export

get_carrying_cap <- function(
  trait_ind,
  trait_opt = default_trait_opt(),
  carrying_cap_opt = default_carrying_cap_opt(),
  carrying_cap_sd = default_carrying_cap_sd()
  ) {
  comrad::testarg_num(trait_opt)
  comrad::testarg_num(carrying_cap_opt)
  comrad::testarg_pos(carrying_cap_opt) # is a nb of ind
  comrad::testarg_num(carrying_cap_sd)
  comrad::testarg_pos(carrying_cap_sd) # is a variance

  trait_dist <- (trait_opt - trait_ind) ^ 2
  if (trait_opt == Inf) {
    trait_dist[which(trait_ind == Inf)] <- 0 # replace NaNs with 0
  }

  carrying_cap <- carrying_cap_opt *
    exp(- (trait_dist / (2 * carrying_cap_sd ^ 2)))

  # Solve possible NaN issues --------------------------------------------------
  # NaNs can arise if both terms in the division are equal to 0 or Inf
  if (carrying_cap_sd == 0) { # carrying_cap_sd has precedence
    nans <- which(trait_dist == 0)
    carrying_cap[nans] <- carrying_cap_opt # as if trait_dist/carrying_cap_sd=0
  } else if (carrying_cap_sd == Inf) {# carrying_cap_sd has precedence
    nans <- which(trait_dist == Inf)
    carrying_cap[nans] <- carrying_cap_opt # as if trait_dist/carrying_cap_sd=0
  }
  # NaNs can also arise if carrying_cap_opt is set to Inf and the exp term is 0
  if (carrying_cap_opt == Inf) { # carrying_cap_opt has precedence
    nans <- which(exp(- (trait_dist / (2 * carrying_cap_sd ^ 2))) == 0)
    carrying_cap[nans] <- carrying_cap_opt
  }

  comrad::testarg_num(carrying_cap) # catch NAs, NaNs and NULL
  comrad::testarg_pos(carrying_cap)
  comrad::testarg_length(carrying_cap, length(trait_ind))

  return(carrying_cap)
}
