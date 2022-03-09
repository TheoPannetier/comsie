#' Read the sequence of events in a simulation from its log
#'
#' @param path_to_log character, path to the log file
#'
#' @retura a tibble with variables t, species, ancestor, event
#' @export
read_events_from_log <- function(path_to_log) {
  lines <- readr::read_lines(path_to_log)
  time_str <- "Sampled generation ([:digit:]+) / [:digit:]+"
  sampling_times <- as.numeric(stringr::str_match(lines, time_str)[, 2])

  # Read immigration
  immig_str <- "Species (#[:alnum:]{6}) immigrated on the island."
  immig_events <- stringr::str_match(lines, immig_str)[, 2]
  immig_tbl <- tibble::tibble(
    "t" = sampling_times,
    "species" = immig_events,
    "ancestor" = immig_events,
    "event" = "immigration"
  ) %>%
    # Exclude lines that are neither time or immigration
    dplyr::filter(!(is.na(t) & is.na(species)))
  # Find time corresponding to each immigration event
  while(any(is.na(immig_tbl$t))) {
    immig_tbl <- immig_tbl %>% mutate(
      t = ifelse(is.na(t), lead(t), t)
    )
  }
  immig_tbl <- immig_tbl[!is.na(immig_tbl$species), ]

  # Read cladogenesis
  clado_str <- "Species (#[:alnum:]{6}) split from species (#[:alnum:]{6})"
  clado_events <- stringr::str_match(lines, clado_str)[, 2:3]
  clado_tbl <- tibble::tibble(
    "t" = sampling_times,
    "species" = clado_events[, 1],
    "ancestor" = clado_events[, 2],
    "event" = "cladogenesis"
  ) %>%
    # Exclude lines that are neither time or immigration
    dplyr::filter(!(is.na(t) & is.na(species)))
  # Find time corresponding to each immigration event
  while(any(is.na(clado_tbl$t))) {
    clado_tbl <- clado_tbl %>% mutate(
      t = ifelse(is.na(t), lead(t), t)
    )
  }
  clado_tbl <- clado_tbl[!is.na(clado_tbl$species), ]

  # Read anagenesis
  anag_str <- "Species (#[:alnum:]{6}) turned into species (#[:alnum:]{6}) by anagenesis."
  anag_events <- stringr::str_match(lines, anag_str)[, 2:3]
  anag_tbl <- tibble::tibble(
    "t" = sampling_times,
    "species" = anag_events[, 2],
    "ancestor" = anag_events[, 1],
    "event" = "anagenesis"
  )  %>%
    # Exclude lines that are neither time or immigration
    dplyr::filter(!(is.na(t) & is.na(species)))
  # Find time corresponding to each immigration event
  while(any(is.na(anag_tbl$t))) {
    anag_tbl <- anag_tbl %>% mutate(
      t = ifelse(is.na(t), lead(t), t)
    )
  }
  anag_tbl <- anag_tbl[!is.na(anag_tbl$species), ]

  event_tbl <- dplyr::bind_rows(
    immig_tbl, anag_tbl, clado_tbl
  ) %>% arrange(t)
  return(event_tbl)
}
