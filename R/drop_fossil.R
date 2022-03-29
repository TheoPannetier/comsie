drop_fossil <- function(phylo, keep_stem = TRUE) {
  # Must do manually bc ape::drop.fossil drops the stem branch :(((
  phylo_alive <- ape::drop.fossil(phylo)
  still_has_stem <- ape::Ntip(phylo_alive) == phylo_alive$Nnode
  if (!keep_stem | still_has_stem) {
    # Then ape is doing a good job already
    return(phylo_alive)
  }
  # Get stem age
  n <- ape::Ntip(phylo)
  dist_to_stem <- ape::dist.nodes(phylo)[n + 1, ][1:n]
  stem_age <- max(dist_to_stem)

  # Get crown age
  n_alive <- ape::Ntip(phylo_alive)
  dist_to_crown <- ape::dist.nodes(phylo_alive)[n_alive + 1, ][1:n_alive]
  crown_age <- max(dist_to_crown)

  # Get stem branch length
  stem_length <- stem_age - crown_age

  # Repair stem
  if (n_alive == 1) {
    # Only one lineage going from stem age to present
    phylo_alive$edge.length <- stem_age
  } else if (n_alive > 1) {
    # Graft stem on
    phylo_alive$edge <- ifelse(phylo_alive$edge > n_alive, phylo_alive$edge + 1, phylo_alive$edge)
    phylo_alive$edge <- matrix(c(n_alive + 1, phylo_alive$edge[, 1],  n_alive + 2, phylo_alive$edge[, 2]), ncol = 2)
    phylo_alive$edge.length <- c(stem_length, phylo_alive$edge.length)
    phylo_alive$Nnode <- phylo_alive$Nnode + 1
  }
  return(phylo_alive)
}
