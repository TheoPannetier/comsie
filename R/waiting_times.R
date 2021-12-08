#' Extract times to events from a phylogeny
#'
#' Turns the phylogeny into a lineage-through-time table, and for which
#' event computes the time to next event.
#'
#' @param phylo an object of the class `phylo` as introduced in
#' [ape][ape::read.tree]. The phylogeny must start with the crown node
#' (not stem), and be binary (no hard polytomies).#'
#' @return a `tibble` with a row per event and four columns:
#'
#' * `time`, time of an event
#' * `N`, number of species in the tree at that time
#' * `next_event`, type of the next event; either `"speciation"` or
#' `"extinction"`
#' * `waiting_time`, time to the next event
#' * `is_speciation`, lgl shortcut to type of event
#'
#' @author Théo Pannetier
#' @export

waiting_times <- function(phylo) {
  if (!class(phylo) == "phylo") {
    stop("'phylo' must be a 'phylo' object.")
  }

  # nolint start
  next_event <- NULL # no NOTE
  time <- NULL # no NOTE
  n_diff <- NULL
  # nolint end

  # Get time and N from phylobates
  ltt_tbl <- phylo %>%
    get_ltt_tbl() %>%
    dplyr::mutate("time" = time - min(time))
  # Drop last row (present)
  ltt_tbl <- ltt_tbl[-nrow(ltt_tbl), ]

  max_time <- range(ltt_tbl$time) %>% diff() %>% round(3)

  wt_tbl <- ltt_tbl %>%
    # What is next event ?
    dplyr::mutate(
      "n_diff" = dplyr::lead(ltt_tbl$N) - ltt_tbl$N,
      "next_event" = dplyr::case_when(
        n_diff >= 1   ~ "speciation",
        n_diff <= -1  ~ "extinction",
        is.na(n_diff) ~ "present"
      )
    ) %>%
    dplyr::select(-n_diff)

  wt_tbl <- wt_tbl %>%
    # Waiting time to next event
    dplyr::mutate("waiting_time" = dplyr::lead(time) - time) %>%
    # Drop last row, waiting time cut by present
    dplyr::filter(next_event != "present") %>%
    dplyr::mutate(
      "is_speciation" = dplyr::case_when(
        next_event == "speciation" ~ TRUE,
        next_event == "extinction" ~ FALSE
        # NA if neither
      )
    )

  # Control output
  if (any(is.na(wt_tbl))) {
    stop("NA(s) found in waiting_times() output")
  }
  if (any(wt_tbl$waiting_time %>% round(3) > max_time)) {
    stop("Some waiting time(s) exceeded total generation span")
  }
  if (any(is.na(wt_tbl$is_speciation))) {
    stop("Some event is neither speciation or extinction")
  }

  return(wt_tbl)
}
