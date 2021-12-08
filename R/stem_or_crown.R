#' Find wheter origin is stem or crown lineages
#'
#' `phylo` is rooted on its stem if nb nodes = nb tips, and on its crown
#' otherwise
#'
#' @param phylo an object of the class `phylo` as introduced in
#' [ape][ape::read.tree]. The phylogeny must start with the crown node
#' (not stem), and be binary (no hard polytomies).#'
#'
#' @author Théo Pannetier
#' @export
stem_or_crown <- function(phylo) {
  ifelse(
    ape::Nnode(phylo) == ape::Ntip(phylo),
    "stem",
    "crown"
  )
}
