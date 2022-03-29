get_all_colonisations <- function(phylo_ls, island_age) {
  all_colonisations <- phylo_ls %>% purrr::imap(function(ls, mainland_sp) {
    all_colonisations_this_sp <- ls %>% purrr::map(function(phylo) {
      event_times <- c(island_age, ape::branching.times(phylo))
      names(event_times) <- NULL
      species_type <- read_species_type(phylo, mainland_sp)
      return(list("event_times" = event_times, "species_type" = species_type))
    })
  })
  return(all_colonisations)
}

read_species_type <- function(phylo, mainland_sp) {
  ntips <- ape::Ntip(phylo)
  species_type <- ifelse(ntips > 1, "C", ifelse(phylo$tip.label == mainland_sp, "I", "A"))
  return(species_type)
}

get_stac <- function(spp, mainland_sp) {
  stac <- ifelse(!mainland_sp %in% spp,
                 2, # Endemic
                 ifelse(length(spp) > 1,
                        3, # Endemic&Non_Endemic
                        4 # Non_Endemic
                 )
  )
  return(stac)
}
