#' Test model adequacy using the tests of Louca & Pennell (2021)
#'
#' Test whether a DD model is consistent with a set of "empirical" (i.e., comrad) trees.
#' The test compares the CDF for either node ages or edge lengths of each empirical tree and the
#' set of bootstrap trees.
#'
#'@param phylos_empirical a list of empirical (i.e. simulated under comrad) trees.
#'@param phylos_bootstrap a list of bootstrap (i.e. simulated under a DD model) trees.
#'@param data_type the type of data to consider for the test, either `"edge_length"` or `"node_age"`.
#'
#' @return a vector of "p-values" for each "empirical" tree. Those metrics are not really p-values but fit
#' the same role: how likely to observe this CDF assuming the empirical trees were simulated under the same model as
#' the bootstrap trees?
#'
#' @export
#' @author Theo Pannetier
#'
test_dd_adequacy_castor <- function(phylos_empirical, phylos_bootstrap, data_type) {
  if (!data_type %in% c("edge_length", "node_age")) {
    stop("\"data_type\" should only be \"edge_length\" or \"node_age\".")
  }
  bootstraps <- phylos_bootstrap %>% purrr::map(function(phylo) {
    extract_phylo_data(phylo = phylo, data_type = data_type)
  })
  # Run the test with every comrad tree
  names(phylos_empirical) <- NULL
  pvals <- purrr::imap_dbl(phylos_empirical, function(phylo_empirical, i) {
    cat("Running bootstrap test for tree", i, "/", length(phylos_empirical), "\n")
    empirical <- extract_phylo_data(phylo = phylo_empirical, data_type = data_type)
    ks_test <- castor:::bootstrap_Kolmogorov_Smirnov_test(
      empirical = empirical,
      bootstraps = bootstraps
    )
    return(ks_test$Pvalue)
  })
  return(pvals)
}

#' Extract data from phylogeny
#'
#' Helper function to `test_dd_adequacy_castor()`. Extracts node ages or edge
#' lengths from a phylogeny
#'
#' @param phylo a `phylo` object
#' @param data_type either `"node_age"` or `"branch_length"`.
#'
#' @return a vector of the data extracted from the tree.
#'
extract_phylo_data <- function(phylo, data_type) {
  if (data_type == "edge_length") {
    return(phylo$edge.length)
  } else if (data_type == "node_age") {
    root_age <- get_root_age(phylo, present_time = 0)
    Ntips <- ape::Ntip(phylo)
    node_ages <- root_age - castor::get_all_distances_to_root(phylo)[(1 + Ntips):(2 * Ntips - 1)]
    return(node_ages)
  }
}
