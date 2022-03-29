split_comsie_tbl <- function(comsie_tbl) {
  comsie_tbl <- comsie_tbl %>%
    group_by(mainland_sp)

  mainland_sp_names <- group_keys(comsie_tbl) %>% pull(mainland_sp)

  comsie_ls <- comsie_tbl %>%
    group_split(.keep = FALSE)
  names(comsie_ls) <- mainland_sp_names

  comsie_ls <- comsie_ls %>%
    map(group_by, immig_nb) %>%
    map(group_split, .keep = FALSE)
  return(comsie_ls)
}
