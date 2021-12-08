#' Apply random mutations to a community
#'
#' The trait value of each individual in the input community is modified, with
#' probability `prob_mutation`, by a mutation sampled in a normal distribution
#' of mean `0` and standard deviation `mutation_sd`.
#'
#' @inheritParams default_params_doc
#'
#' @author Theo Pannetier
#' @export

apply_mutations <- function(
  traits_comm,
  prob_mutation = default_prob_mutation(),
  mutation_sd = default_mutation_sd()
) {
  # Test arguments -------------------------------------------------------------
  comrad::testarg_num(prob_mutation)
  comrad::testarg_prop(prob_mutation)
  comrad::testarg_num(mutation_sd)
  comrad::testarg_pos(mutation_sd)

  # Apply mutations ------------------------------------------------------------
  is_mutant <- stats::rbinom(length(traits_comm), 1, prob_mutation)
  mutations <- stats::rnorm(length(traits_comm), 0, mutation_sd)
  traits_comm <- ifelse(
    is_mutant,
    traits_comm + mutations,
    traits_comm
  )

  # Test output --------------------------------------------------------------
  comrad::testarg_num(traits_comm)

  return(traits_comm)
}
