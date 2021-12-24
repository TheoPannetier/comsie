#' Assert a function argument is of correct type
#'
#' @param arg value of the asserted argument.
#'
#' @author Theo Pannetier
#'
#' @name testargs
NULL

#' @inheritParams default_params_doc
#' @export
#' @rdname testargs
test_island_comm <- function(comm) {
  if (!tibble::is_tibble(comm)) {
    stop("'", substitute(comm), "' should be a tibble.")
  }
  if (length(attributes(comm)$row.names) == 0) {
    stop("'", substitute(comm), "' is empty.")
  }
  if (!length(attributes(comm)$names) %in% 3:4) {
    stop("'", substitute(comm), "' should have 3 or 4 columns.")
  }
  if (any(!attributes(comm)$names %in% c("z", "species", "ancestral_species", "root_species"))) {
    stop(
      "'", substitute(comm),
      "' should have columns 'z', 'species', 'ancestral_species' and (optionally) 'root_species'."
    )
  }
  col_classes <- c(
    class(comm$z), class(comm$species), class(comm$ancestral_species)
  )
  if (any(col_classes != c("numeric", "character", "character"))) {
    stop(
      "'", substitute(comm),
      "' col classes should be numeric, character, and character respectively."
    )
  }
}

#' @param comsie_tbl a tibble containing the output of a `comrad` simulation,
#' as produced by [run_simulation()].
#' @export
#' @rdname testargs
test_comsie_tbl <- function(comsie_tbl) {
  if (!tibble::is_tibble(comsie_tbl)) {
    stop("'", substitute(comsie_tbl), "' is not a tibble.")
  }
  if (length(attributes(comsie_tbl)$row.names) == 0) {
    stop("'", substitute(comsie_tbl), "' is empty.")
  }
  if (!length(attributes(comsie_tbl)$names) %in% 4:5) {
    stop("'", substitute(comsie_tbl), "' should have 4 or 5 columns.")
  }
  if (any(
    !attributes(comsie_tbl)$names %in% c("t", "z", "species", "ancestral_species", "root_species")
  )) {
    stop(
      "'", substitute(comsie_tbl),
      "' should have columns 't', z', 'species', 'ancestral_species' and (optionally) 'root_species'."
    )
  }
  col_classes <- c(
    class(comsie_tbl$t),
    class(comsie_tbl$z),
    class(comsie_tbl$species),
    class(comsie_tbl$ancestral_species)
  )
  if (any(col_classes != c("numeric", "numeric", "character", "character"))) {
    stop(
      "'", substitute(comsie_tbl),
      "' col classes should be numeric, character and character, respectively."
    )
  }
  if (ncol(comsie_tbl) == 5 && !class(comsie_tbl$root_species) == "character") {
    stop(
      "'", substitute(comsie_tbl),
      "' col classes should be numeric, character, character and character, respectively."
    )
  }
}
