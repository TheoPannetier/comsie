#' Extracts phylogenetic information from a `comrad` simulation
#'
#' Reads the output of a `comrad` simulation and creates a table containing
#' the necessary information about every species to make a phylogeny.
#'
#' @inheritParams default_params_doc
#'
#' @return a tibble with four columns and a row per species in the simulation
#' * `species_name`, name of the species
#' * `ancestor_name`, name of the ancestral species from which it branched. `NA`
#' for the initial species.
#' * `time_birth`, the generation at which the species appeared.
#' * `time_death`, the generation at which the last individual died. Last
#' generation if the species was still extant at the end of the simulation.
#'
#' @author Théo Pannetier
#' @export

build_comsie_spp_tbl <- function(comsie_tbl) {

  spp <- unique(comsie_tbl$species)
  founder <- unique(comsie_tbl$species[is.na(comsie_tbl$ancestral_species)])
  if (length(founder) == 0) {
    stop("No founder could be found for this clade")
  } else if (length(founder) > 1) {
    stop("This clade appears to have more than one founder")
  }
  names(spp) <- spp # for automated labelling by map_dfr
  time_seq <- sort(unique(comsie_tbl$t))

  spp_tbl <- purrr::map_dfr(
    spp,
    function(sp) {
      is_sp <- comsie_tbl$species == sp
      time_range <- unique(comsie_tbl$t[is_sp])
      time_birth <- min(time_range)
      occur_last <- max(time_range)
      time_death <- ifelse(
        occur_last == max(time_seq), # is present?
        occur_last, # not extinct at present
        time_seq[which(time_seq == occur_last) + 1] # extinct at first non-occurrence
      )
      ancestor_name <- unique(
        comsie_tbl$ancestral_species[is_sp]
      )
      sp_entry <- tibble::tibble(
        "ancestor_name" = ancestor_name,
        "time_birth" = time_birth,
        "time_death" = time_death
      )
    },
    .id = "species_name"
  )
  return(spp_tbl)
}
