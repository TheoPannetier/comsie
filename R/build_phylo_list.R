build_phylo_list <- function(comsie_tbl, with_fossil = TRUE) {

  # Nest by clade and colonisation event
  comsie_ls <- comsie::split_comsie_tbl(comsie_tbl)

  # Convert into species-level diversity record
  comsie_ls_spp <- comsie_ls %>%
    purrr::map(function(clade_ls) {
    clade_ls %>% purrr::map(comsie::build_comsie_spp_tbl)
  })

  # Build phylos
  newick_ls <- comsie_ls_spp %>% purrr::map(comsie::write_newick_str_comsie)
  phylo_ls <- newick_ls %>%
    purrr::map(comsie::newick_to_phylo, with_fossil = with_fossil)
  return(phylo_ls)
}
