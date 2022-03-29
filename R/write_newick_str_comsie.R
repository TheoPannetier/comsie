build_newick_tbl <- function(spp_tbl) {
  # species must be ordered by chronological order
  newick_tbl <- spp_tbl[order(spp_tbl$time_birth), ]
  # Check all ancestors are present
  if (
    any(
      !newick_tbl$ancestor_name[!is.na(newick_tbl$ancestor_name)] %in%
      newick_tbl$species_name)
  ) {
    stop("One or more ancestors absent from species record.")
  }
  # Identify sister taxa
  newick_tbl$sister_name <- map_chr(1:nrow(newick_tbl), function (i) {
    parent <- newick_tbl$ancestor_name[i]
    t_birth <- newick_tbl$time_birth[i]
    sister_name <- newick_tbl$species_name[-i][which(
      # Same parent, same birth time
      newick_tbl$ancestor_name[-i] == parent &
        newick_tbl$time_birth[-i] == t_birth
    )]
    nb_sisters <- length(sister_name)
    if (nb_sisters == 1) {
      return(sister_name)
    } else if (nb_sisters == 0) {
      return(as.character(NA))
    } else if (nb_sisters > 1) {
      stop(glue::glue("Species {newick_tbl$species_name[i]} has more than one sister species."))
    }
  })

  # Concatenate Newick string recursively
  while (nrow(newick_tbl) > 1) {
    # Process tips from youngest to oldest
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

      # Graft child onto parent
      parent_sister_name <- newick_tbl$sister_name[parent]
      if (!is.na(parent_sister_name)) {
        parent_sister <- which(newick_tbl$species_name == parent_sister_name)
        if (length(parent_sister) != 1) {
          stop(glue::glue("Sister {parent_sister_name} of parent {parent_name} could not be found"))
        }
        newick_tbl$sister_name[parent_sister] <- newick_tbl$species_name[child]
      }
      newick_tbl$species_name[parent] <- newick_tbl$species_name[child]
      newick_tbl$time_death[parent] <- newick_tbl$time_death[child]
      newick_tbl <- newick_tbl[-child, ]

    } else {

      # Cladogenesis
      sister <- which(newick_tbl$species_name == sister_name)
      string_child <- paste0(
        child_name, ":",
        newick_tbl$time_death[child] - newick_tbl$time_birth[child]
      )
      string_sister <- paste0(
        newick_tbl$species_name[sister], ":",
        newick_tbl$time_death[sister] - newick_tbl$time_birth[sister]
      )
      newick_str <- paste0("(", string_sister, ",", string_child, ")")
      # Graft both sisters' newick onto parent species
      parent_sister_name <- newick_tbl$sister_name[parent]

      if (!is.na(parent_sister_name)) {
        parent_sister <- which(newick_tbl$species_name == parent_sister_name)
        if (length(parent_sister) != 1) {
          stop(glue::glue("Sister {parent_sister_name} of parent {parent_name} could not be found"))
        }
        newick_tbl$sister_name[parent_sister] <- newick_str
      }
      newick_tbl$species_name[parent] <- newick_str
      newick_tbl <- newick_tbl[-c(child, sister), ]
    }
  }
  newick_tbl <- newick_tbl[, -5] # drop sister info
  return(newick_tbl)
}

bind_newick_str_comsie <- function(newick_tbl, mainland_sp) {
  if (any(!is.na(newick_tbl$ancestor_name))) {
    stop("Some ancestors remain beside founders.")
  }

  newick_tbl <- newick_tbl %>%
    dplyr::mutate("contains_mainland_sp" = stringr::str_detect(species_name, mainland_sp))

  if (sum(newick_tbl$contains_mainland_sp) > 1) {
    stop("Test this code block before running")
    newick_tbl <- newick_tbl %>%
      dplyr::group_by(contains_mainland_sp) %>%
      dplyr::mutate("to_drop" = contains_mainland_sp & time_birth != max(time_birth)) %>%
      dplyr::filter(!to_drop)
  }

  while (nrow(newick_tbl) > 1) {
    # Graft last colonisation event on the branch of previous one
    youngest <- which.max(newick_tbl$time_birth)
    previous <-  which.max(newick_tbl$time_birth[-youngest])
    label_youngest <- newick_tbl$species_name[youngest]
    label_previous <- newick_tbl$species_name[previous]

    str_youngest <- paste0(
      label_youngest, ":",
      # Time of divergence is colonisation time of previous immigrant
      newick_tbl$time_death[youngest] - newick_tbl$time_birth[previous]
    )
    str_previous <- paste0(
      label_previous, ":",
      newick_tbl$time_death[previous] - newick_tbl$time_birth[previous]
    )
    newick_str <- paste0("(", str_previous, ",", str_youngest, ")")
    newick_tbl$species_name[previous] <- newick_str
    newick_tbl$time_death[previous] <- newick_tbl$time_birth[previous]
    newick_tbl <- newick_tbl[-youngest,]
  }

  # Add stem and close tree
  newick_str <- paste0(
    "(", newick_tbl$species_name, ":", newick_tbl$time_death - newick_tbl$time_birth, ");"
  )
  return(newick_str)
}
