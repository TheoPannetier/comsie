#' Convert `comrad` simulation results into a phylogeny
#'
#' Reads a table of results and returns a `phylo` class object.
#'
#' @inheritParams default_params_doc
#' @param with_extinct logical, should extinct taxa be included? `TRUE` returns
#' the full tree, `FALSE` returns the reconstructed tree.
#' @param include_stem logical, should the stem lineage and stem age be
#' included? If not, the string stops at crown lineages.

#' @author Théo Pannetier
#' @export

sim_to_phylo <- function(comrad_tbl, include_stem = TRUE, with_extinct = TRUE) {
  comrad_tbl %>%
    dplyr::select(-t) %>%
    comrad::test_comrad_comm()

  newick_str <- comrad_tbl %>%
    comrad::build_spp_tbl() %>%
    comrad::write_newick_str(include_stem = include_stem)

  phylo <- ape::read.tree(text = newick_str)

  if (!with_extinct) {
    phylo <- ape::drop.fossil(phylo)
    if (!include_stem && ape::Ntip(phylo) == 1) {
      stop("can't get a crown tree: only one living lineage in the community")
    }
  }
  return(phylo)
}
