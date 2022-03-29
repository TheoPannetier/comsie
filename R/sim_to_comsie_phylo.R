sim_to_comsie_phylo <- function(comsie_tbl, include_stem = TRUE, with_extinct = TRUE) {
  newick_str <- comsie_tbl %>%
    comrad::build_spp_tbl() %>%
    comsie::write_newick_str_comsie(include_stem = include_stem)

  phylo <- ape::read.tree(text = newick_str)

  if (!with_extinct) {
    n <- ape::Ntip(phylo)
    x <- ape::dist.nodes(phylo)[n + 1, ][1:n]
    phylo <- ape::drop.tip(
      phylo,
      which(x < max(x) - 1e-08),
      collapse.singles = !include_stem
    )
    if (!include_stem && ape::Ntip(phylo) == 1) {
      stop("can't get a crown tree: only one living lineage in the community")
    }
  }
  return(phylo)
}
