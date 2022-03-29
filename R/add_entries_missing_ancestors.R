add_entries_missing_ancestors <- function(spp_tbl, job_id) {
  all_ancestors <- unique(spp_tbl$ancestor_name[!is.na(spp_tbl$ancestor_name)])
  all_spp <- spp_tbl$species_name
  missing_ancestors <- all_ancestors[!all_ancestors %in% all_spp]
  if (length(missing_ancestors) > 0) {
    event_tbl <- read_events_from_log(path_to_log_hd(job_id, pkg = "comsie"))
    while (length(missing_ancestors) > 0) {
      missing_ancestor <- missing_ancestors[1]
      grand_ancestor <- event_tbl$ancestor[event_tbl$species == missing_ancestor]
      if (!grand_ancestor %in% all_spp) {
        stop("Two successive ancestral species are missing.")
      }
      parent_i <- which(spp_tbl$species_name == grand_ancestor)
      sampling_time <- spp_tbl$time_birth[spp_tbl$ancestor_name == missing_ancestor]
      sampling_time <- sampling_time[!is.na(sampling_time)][1]
      sister_i <- which(
        # Same parent, same birth time
        spp_tbl$ancestor_name == grand_ancestor &
        spp_tbl$time_birth == sampling_time
      )
      #spp_tbl$species_name[]
      new_time_birth <- sampling_time - 0.01 # parent birth must be before child birth
      spp_tbl$time_death[parent_i] <- new_time_birth
      spp_tbl$time_birth[sister_i] <- new_time_birth

      spp_tbl <- spp_tbl %>% dplyr::bind_rows(
        tibble(
          "species_name" = missing_ancestor,
          "ancestor_name" = grand_ancestor,
          "time_birth" = new_time_birth,
          "time_death" = sampling_time
        )
      )

      # Update what's missing
      all_ancestors <- unique(spp_tbl$ancestor_name[!is.na(spp_tbl$ancestor_name)])
      all_spp <- spp_tbl$species_name
      missing_ancestors <- all_ancestors[!all_ancestors %in% all_spp]
    }
  }
  return(spp_tbl)
}
