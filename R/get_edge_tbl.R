#' Get a table with node and edge info
#'
#' Returns a table containing the tree topology (parent-to-child node pairs;
#' whether a node is a tip or internal), edge (branch) lengths and timing of
#' nodes (branching times).
#'
#' @param phylo an object of the class `phylo` as introduced in
#' [ape][ape::read.tree]. The phylogeny must start with the crown node
#' (not stem), and be binary (no hard polytomies).
#' @return a `tibble` with one observation per branch and five variables:
#'  * `parent_node` ID of the parent node
#'  * `child_node` ID of the child node
#'  * `is_tip` logical, is the (child) node a tip (`TRUE`) of the tree or an
#'  internal node (`FALSE`)
#'  * `edge_length` numeric length of the edge between parent and child,
#'  i.e. the branch length
#'  * `time` for timetrees, the time of the node relative to present (taken as
#'  the latest tip(s) in the tree)
#'
#' @author Théo Pannetier
#' @export
#'
get_edge_tbl <- function(phylo) {

  if (!ape::is.rooted(phylo)) {
    stop("\"phylo\" must be rooted.")
  }

  . <- NULL
  child_node <- NULL # no NOTE
  time_child <- NULL # no NOTE
  edge_length <- NULL # no NOTE

  ntips <- length(phylo$tip.label)
  root_node <- ntips + 1

  edge_tbl <- tibble::tibble(
    "parent_node" = phylo$edge[, 1],
    "child_node" = phylo$edge[, 2],
    "is_tip" = child_node <= ntips,
    "edge_length" = phylo$edge.length
  ) %>%
    # entry for root node
    dplyr::bind_rows(
      tibble::tibble(
        "parent_node" = NA,
        "child_node" = root_node,
        "is_tip" = FALSE,
        "edge_length" = 0
      ),
      . # root comes first
    ) %>%
    # initialise times
    dplyr::mutate(
      "time_child" = ifelse(child_node == root_node, 0, NA)
    )

  # fill times
  for (i in 2:nrow(edge_tbl)) { # skip first row (root)
    parent_i <- which(edge_tbl$child_node == edge_tbl$parent_node[i])
    edge_tbl$time_child[i] <- edge_tbl$edge_length[i] +
      edge_tbl$time_child[parent_i]
  }

  # transform times relative to present
  root_age <- max(edge_tbl$time_child)
  edge_tbl <- edge_tbl %>%
    dplyr::mutate(
      time_child = time_child - root_age %>% round(3),
      time_parent = time_child - edge_length %>% round(3)
    )

  return(edge_tbl)
}
