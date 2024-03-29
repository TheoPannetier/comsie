#' Resolve cladogenetic and anagetic speciation in the island community
#'
#' Scan the island community and if conditions for cladogenetic speciation are
#' met, splits species accordingly. Then does the same for anagenetic speciation.
#'
#' @details **For cladogenesis**:
#' For each species, check if there is any gap `>= trait_dist_sp` in trait
#' values, and split the relevant species in two when one is found. The less
#' numerous half becomes the new species.
#'
#' **For anagenesis**:
#' Only concerns non-endemic species, i.e. species that are still the same as
#' one of the populations in `mainland_comm`. Such a species becomes a new
#' species if the *average* trait value of its members differs from the average
#' trait value of the mainland population by `trait_dist_sp`.
#'
#' @inheritParams default_params_doc
#' @note `apply_speciation()` can currently only split one species into two. If
#' multiple gaps emerge in a species at a single time step, only the first one
#' will be treated. As long as branching does not happen at every generation,
#' this is unlikely to be an issue, because an unresolved gap will be caught on
#' the next time step. In phylogenetic terms, this results in polytomies being
#' resolved as soft polytomies.
#' Besides, simultaneous branching events in different species are handled
#' perfectly fine.
#'
#' @author Théo Pannetier
#' @export

apply_speciation <- function(island_comm,
                             mainland_comm,
                             used_species_names,
                             trait_dist_sp = comrad::default_trait_dist_sp()
                             ) {

  existing_sp_names <- unique(c(island_comm$species, mainland_comm$species))

  comsie::test_island_comm(island_comm)

  # Resolve cladogenetic speciation
  island_comm <- island_comm[order(island_comm$z), ]
  spp <- unique(island_comm$species)

  for (sp in spp) {
    # Keep track of species members' position
    where <- which(island_comm$species == sp)
    # Extract members of focal species
    sp_members <- island_comm[island_comm$species == sp, ]
    nb_inds <- length(sp_members$z)

    # Check for gaps in trait values -------------------------------------------
    gaps <- comrad::find_trait_gaps(
      traits =  sp_members$z,
      trait_dist_sp = trait_dist_sp
    )

    if (length(gaps) > 0) {
      gap <- gaps[1] # only the first gap is treated for now (soft polytomy)

      # Split species in two ---------------------------------------------------
      new_sp <- NULL
      while (any(is.null(new_sp)) || any(new_sp %in% used_species_names) || any(duplicated(new_sp))) {
        new_sp <- charlatan::ch_hex_color(n = 2)
      }
      used_species_names <- append(used_species_names, new_sp)
      cat("\nSpecies", new_sp[1], "and", new_sp[2], "split from species", sp)

      # Both become new species
      sp_labels <- rep(as.numeric(NA), nb_inds)
      sp_labels[1:gap] <- new_sp[1]
      sp_labels[(gap + 1):length(sp_labels)] <- new_sp[2]

      # Test format & update island_community ---------------------------------
      comrad::testarg_char(sp_labels)
      comrad::testarg_length(sp_labels, length(sp_members$species))
      island_comm$ancestral_species[where] <- sp
      island_comm$species[where] <- sp_labels
    }
  }

  # Resolve anagenetic speciation

  # Extract endemic populations not differentiated from their mainland ancestor
  is_non_endemic <- island_comm$species %in% c(mainland_comm$species)
  non_endemic_species <- unique(island_comm$species[is_non_endemic])

  for (sp in non_endemic_species) {
    # Keep track of species members' position
    where <- which(island_comm$species == sp)
    # Extract members of focal species
    island_mean_z <- mean(island_comm[island_comm$species == sp, ]$z)
    mainland_mean_z <- mainland_comm$mean_z[mainland_comm$species == sp]
    if (abs(island_mean_z - mainland_mean_z) >= trait_dist_sp) {
      new_sp <- NULL
      while (is.null(new_sp) || new_sp %in% used_species_names) {
        new_sp <- charlatan::ch_hex_color()
      }
      cat("\nSpecies", sp, "turned into species", new_sp, "by anagenesis.")
      island_comm$species[where] <- new_sp
      island_comm$ancestral_species[where] <- sp
    }
  }
  return(island_comm)
}
