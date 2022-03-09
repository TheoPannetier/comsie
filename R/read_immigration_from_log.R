#' Read immigration events and their time from a simulation's log
#'
#' @param path_to_log character, path to the log of the simulation
#' @return a tibble with columns "t" and "species"
#'
#' @export
read_immigration_from_log <- function(path_to_log) {
  lines <- readr::read_lines(path_to_log)
  sampling_times <- as.numeric(stringr::str_match(lines, "Sampled generation ([:digit:]+) / [:digit:]+")[, 2])
  immigrants <- stringr::str_match(lines, "Species (#[:alnum:]{6}) immigrated on the island.")[, 2]
  immigration_tbl <- tibble(
    "t" = sampling_times,
    "species" = immigrants
  ) %>%
    # Exclude lines that are neither time or immigration
    dplyr::filter(!(is.na(t) & is.na(species)))
  # Find time corresponding to each immigration event
  while(any(is.na(immigration_tbl$t))) {
    immigration_tbl <- immigration_tbl %>% mutate(
      t = ifelse(is.na(t), lead(t), t)
    )
  }
  immigration_tbl <- immigration_tbl[!is.na(immigration_tbl$species), ]
  return(immigration_tbl)
}
