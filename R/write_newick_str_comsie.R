write_newick_str_comsie <- function(spp_tbl, include_stem = TRUE) {

  # species must be ordered by chronological order
  newick_tbl <- spp_tbl[order(spp_tbl$time_birth), ]
  # only one founder
  if (sum(is.na(newick_tbl$ancestor_name)) > 1) {
    stop("This clade appears to contain more than one founders.")
  }
  if (
    any(
      !newick_tbl$ancestor_name[!is.na(newick_tbl$ancestor_name)] %in%
      newick_tbl$species_name)
  ) {
    stop("One or more ancestors absent from recorded species.")
  }

    # Identify sister taxa
    newick_tbl$sister_name <- map_chr(1:nrow(newick_tbl), function (i) {
      sister_name <- newick_tbl$species_name[-i][which(newick_tbl$ancestor_name[-i] == newick_tbl$ancestor_name[i])]
      nb_sisters <- length(sister_name)
      if (nb_sisters == 1) {
        return(sister_name)
      } else if (nb_sisters == 0) {
        return(as.character(NA))
      } else if (nb_sisters > 1) {
        stop(glue::glue("Species {newick_tbl$species_name[i]} has more than one sister species."))
      }
    })
  # all ancestors present in the table
  #any(!newick_tbl$ancestor_name[!is.na(newick_tbl$ancestor_name)] %in% newick_tbl$species_name)

  # Concatenate Newick string recursively
  while (nrow(newick_tbl) > 1) {

    # Process tips from youngest to oldest
    #child <- which(newick_tbl$time_birth == max(newick_tbl$time_birth))
    child <- which.max(newick_tbl$time_birth)
    child <- max(child, 2) # skip the stem
    parent_name <- newick_tbl$ancestor_name[child]
    child_name <- newick_tbl$species_name[child]

    # Look for tip/clade containing the parent
    parent <- which(newick_tbl$species_name == parent_name)
    if (newick_tbl$time_death[parent] != newick_tbl$time_birth[child]) {
      stop(glue::glue("Parent taxon {parent_name} died after child taxon {child_name} was born."))
    }

    # Write string for this node
    sister_name <- newick_tbl$sister_name[child]
    if (is.na(sister_name)) {

      # Anagenesis
      if (newick_tbl$time_death[parent] != newick_tbl$time_birth[child]) {
        stop(glue::glue("Parent taxon {parent_name} died after child taxon {child_name} was born."))
      }
      parent_sister <- which(newick_tbl$species_name == newick_tbl$sister_name[parent])
      if (!any(is.na(parent_sister))) {
        newick_tbl$sister_name[parent_sister] <- newick_tbl$species_name[child]
      }
      newick_tbl$species_name[parent] <- newick_tbl$species_name[child]
      newick_tbl$time_death[parent] <- newick_tbl$time_death[child]
      newick_tbl <- newick_tbl[-child, ]

    } else {

      # Cladogenesis
      sister <- which(newick_tbl$species_name == sister_name)
      if (newick_tbl$time_birth[sister] != newick_tbl$time_birth[child]) {
        stop(glue::glue("Sister taxa {child_name} and {sister_name} have a different birth time."))
      }
      string_child <- paste0(
        child_name, ":",
        newick_tbl$time_death[child] - newick_tbl$time_birth[child]
      )
      string_sister <- paste0(
        newick_tbl$species_name[sister], ":",
        newick_tbl$time_death[sister] - newick_tbl$time_birth[sister]
      )
      newick_str <- paste0("(", string_sister, ",", string_child, ")")

      parent_sister <- which(newick_tbl$species_name == newick_tbl$sister_name[parent])
      if (!any(is.na(parent_sister))) {
        newick_tbl$sister_name[parent_sister] <- newick_str
      }
      newick_tbl$species_name[parent] <- newick_str
      newick_tbl <- newick_tbl[-c(child, sister), ]
    }
  }

  if (include_stem) {
    newick_str <- paste0(
      "(", newick_tbl$species_name, ":", newick_tbl$time_death, ");"
    )
  } else {
    newick_str <- paste0(
      newick_tbl$species_name, ";"
    )
  }
  return(newick_str)
}
