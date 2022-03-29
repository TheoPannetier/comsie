split_founder_col <- function(comsie_tbl) {
  comsie_tbl <- comsie_tbl %>%
    dplyr::mutate(
      "mainland_sp" = stringr::str_match(founder, "^([:graph:]*)\\_[:digit:]+$")[,2],
      "immig_nb" = as.integer(stringr::str_match(founder, "^[:graph:]*\\_([:digit:]+)$")[,2])
    ) %>%
    dplyr::select(-founder)
  return(comsie_tbl)
}
