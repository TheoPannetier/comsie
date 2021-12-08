#' Plot Akaike weights as a pie chart
#'
#' @param aicw_tbl a data frame
#' @param slice_by either "dd_model" or "speciation_func", "extinction_func",
#' how the AICw scores should be summarised and the pie chart coloured.
#' @param by_tree logical, does the data contain results for each tree separately (`TRUE`)
#' or for a set of replicate trees as a whole (`FALSE`)?
#'
#' @export
plot_aicw_pie <- function(aicw_tbl, slice_by = "dd_model", by_tree = FALSE) {
  if (!slice_by %in% c("dd_model", "speciation_func", "extinction_func")) {
    stop("which_rate should be either dd_model, speciation_func or extinction_func")
  }
  if (by_tree) {
    if (!"tree" %in%  colnames(aicw_tbl)) {
      stop("if by_tree = TRUE, aicw_tbl must contain a variable called tree")
    }
    aicw_tbl <- summarise_aicw_over_trees(aicw_tbl)
  }


  if (slice_by == "dd_model") {
    aicw_tbl <- aicw_tbl %>%
      dplyr::rename("grouping_var" = dd_model)
  } else if (slice_by == "speciation_func") {
    aicw_tbl <- aicw_tbl %>%
      dplyr::mutate(
        "grouping_var" = dd_model_to_speciation_func(dd_model),
      )
  } else { # slice_by == "extinction_func"
    aicw_tbl <- aicw_tbl %>%
      dplyr::mutate(
        "grouping_var" = dd_model_to_extinction_func(dd_model),
      )
  }

  aicw_tbl <- aicw_tbl %>%
    dplyr::group_by(grouping_var) %>%
    summarise("total_aicw" = sum(aicw))

  if (slice_by == "dd_model") {
    colour_palette <- dd_model_colours()
  } else {
    colour_palette <- dd_func_colours()
  }

  aicw_tbl %>%
    ggplot(aes(x = "", y = total_aicw, fill = grouping_var)) +
    geom_bar(
      stat = "identity",
      width = 1,
      colour = "white",
      show.legend = FALSE
    ) +
    coord_polar("y", start = 0) +
    theme_void() +
    scale_fill_manual(
      values = colour_palette
    )
}
