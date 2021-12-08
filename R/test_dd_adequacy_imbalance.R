#' Test DD model adequacy on tree imbalance
#'
#' Test whether the imbalalance pattern of a set of "empirical" (comrad) trees
#' is consistent with that predicted by a DD model using the Sackin or Colless indices,
#' via [castor::tree_imbalance()].
#'
#' @param phylos_empirical a list of empirical (i.e. simulated under comrad) trees.
#' @param phylos_bootstrap a list of bootstrap (i.e. simulated under a DD model) trees.
#' @param type the index to use to measure tree imbalance. Either `"Sackin"` or `"Colless"`.
#'
#' @return a list with four elements:
#' - `delta_imbalance_empirical` vector with the absolute difference in tree
#' imbalance between every tree in the empirical set and the average bootstrap LTT
#' - `delta_imbalance_bootstrap` vector with the absolute difference in tree
#' imbalance  between every tree in the bootsrap set and the average bootstrap LTT
#' - `p` a measure akin to a p-value, computed as the proportion of bootstrap
#' trees for which `delta_imbalance_bootstrap` > `delta_imbalance_empirical`.
#' This measure is computed once for every tree in the empirical set.
#' - `ks` output of a two-sample, one-sided (empirical > bootstrap)
#' Kolmogorov-Smirnov test on the distribution of the delta imbalance of the
#' empirical and bootsrap test.
#'
#' @author Theo Pannetier
#' @export
#'

test_dd_adequacy_imbalance <- function(phylos_empirical, phylos_bootstrap, type = "Sackin") {
  if (!type %in% c("Sackin", "Colless")) {
    stop("\"type\" should only be \"Sackin\" or \"Colless\"")
  }
  imbalance_empirical <- phylos_empirical %>% purrr::map_dbl(castor::tree_imbalance, type = type)
  imbalance_bootstrap <- phylos_bootstrap %>% purrr::map_dbl(castor::tree_imbalance, type = type)
  avg_imbalance_bootstrap <- mean(imbalance_bootstrap)

  delta_imbalance_bootstrap <- abs(imbalance_bootstrap - avg_imbalance_bootstrap)
  delta_imbalance_empirical <- abs(imbalance_empirical - avg_imbalance_bootstrap)

  pvals <- delta_imbalance_empirical %>% purrr::map_dbl(function(empirical) {
    sum(delta_imbalance_bootstrap > empirical) / length(delta_imbalance_bootstrap)
  })

  ks_test <- stats::ks.test(
    x = delta_imbalance_empirical,
    y = delta_imbalance_bootstrap,
    alternative = "less"
    )
  return(
    list(
      "imbalance_empirical" = imbalance_empirical,
      "imbalance_bootstrap" = imbalance_bootstrap,
      "delta_imbalance_empirical" = delta_imbalance_empirical,
      "delta_imbalance_bootstrap" = delta_imbalance_bootstrap,
      "pvals" = pvals,
      "ks_test" = ks_test
    )
  )
}

