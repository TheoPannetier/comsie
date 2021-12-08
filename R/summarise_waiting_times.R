#' Summarise waiting times data set
#'
#' Summarise waiting times observations using a summary function
#' (either mean or median), to get one single observation per tree, value of N
#' and type of event (speciation or extinction)
#'
#' @param waiting_times_tbl the output of [waiting_times()] for several trees,
#' with an extra column `replicate` that indicates which tree each waiting time is from.
#' @param summary_func character,  the function to summarise the data with. Either
#' `mean` or `median`.
#'
#' @return a `tibble` with variables `replicate`, `next_event`,
#' `N`, `waiting_time` and `is_speciation`
#'
#' @export
#' @author Theo Pannetier
#'
summarise_waiting_times <- function(
  waiting_times_tbl,
  summary_func = mean
) {
  if (!"replicate" %in% colnames(waiting_times_tbl)) {
    stop("There should be a variable called \"replicate\" in waiting_times_tbl.")
  }
  summd_wt_tbl <- waiting_times_tbl %>%
    dplyr::group_by(replicate, next_event, N) %>%
    dplyr::summarise(
    "waiting_time" = summary_func(waiting_time),
    "is_speciation" = is_speciation,
    .groups = "drop"
  ) %>%
    dplyr::distinct()
  return(summd_wt_tbl)
}
