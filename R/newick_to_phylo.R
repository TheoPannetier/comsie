newick_to_phylo <- function(newick_str, with_fossil = TRUE) {

  phylo <- ape::read.tree(text = newick_str)

  if (!with_fossil) {
    phylo <- drop_fossil(phylo) # trust no ape
  }
  return(phylo)
}
