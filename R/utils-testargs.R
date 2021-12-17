#' Assert a function argument is of correct type
#'
#' \itemize{
#'  \item \code{testarg_num()} asserts that the argument is a numeric value.
#'  Also rejects \code{NaNs} and \code{NAs} by default.
#'  \item \code{testarg_prop()} asserts that the argument value lies between 0
#'  and 1.
#'  \item \code{testarg_pos()} asserts that the argument is positive.
#'  \item \code{testarg_not_this()} asserts that the argument is different
#'  from one or several forbidden values.
#'  \item \code{testarg_char()} asserts that the argument is a character.
#'  \item \code{testarg_log()} asserts that the argument is a logical.
#'  \item \code{testarg_length()} asserts that the argument has the correct
#'  length.
#'  \item \code{test_comrad_comm()} asserts the format of the supplied community
#'  is standard.
#' }
#'
#' @param arg value of the asserted argument.
#'
#' @author Theo Pannetier
#'
#' @name testargs
NULL

#' @export
#' @rdname testargs
#' @param allow_nan logical, should NaNs pass the test?
#' @param allow_na logical, should NAs pass the test?
testarg_num <- function(arg, allow_nan = FALSE, allow_na = FALSE) {
  if (!is.numeric(arg)) {
    stop("'", substitute(arg), "' must be numeric")
  } else if (!allow_nan && any(is.nan(arg))) {
    stop("'", substitute(arg), "' contains one or more NaNs")
  } else if (!allow_na && any(is.na(arg))) {
    stop("'", substitute(arg), "' contains one or more NAs")
  } else if (length(arg) < 1) {
    stop("'", substitute(arg), "' is empty")
  }
}

#' @export
#' @rdname testargs
testarg_int <- function(arg) {
  if (arg %% 1 != 0) {
    stop("'", substitute(arg), "' must be an integer")
  }
}

#' @export
#' @rdname testargs
testarg_pos <- function(arg) {
  if (any(arg < 0)) {
    stop("'", substitute(arg), "' must be a positive numeric")
  }
}

#' @export
#' @rdname testargs
testarg_prop <- function(arg) {
  if (any(!dplyr::between(arg, 0, 1))) {
    stop("'", substitute(arg), "' must be a numeric between 0 and 1")
  }
}

#' @export
#' @rdname testargs
#' @param forbidden a vector containing forbidden values.
#' @details Currenty \code{testarg_not_this()} cannot be tested properly.
testarg_not_this <- function(arg, forbidden) {
  if (any(arg %in% forbidden)) {
    stop("'", substitute(arg), "' contains forbidden values: ",
         paste(intersect(arg, forbidden), collapse = " "))
  }
}

#' @export
#' @rdname testargs
testarg_char <- function(arg) {
  if (!is.character(arg)) {
    stop("'", substitute(arg), "' must be a character.")
  } else if (length(arg) < 1) {
    stop("'", substitute(arg), "' is empty")
  }
}

#' @export
#' @rdname testargs
testarg_log <- function(arg) {
  if (!is.logical(arg)) {
    stop("'", substitute(arg), "' must be a logical")
  } else if (length(arg) < 1) {
    stop("'", substitute(arg), "' is empty")
  }
}
#' @export
#' @rdname testargs
#' @param correct_length numeric, the length the argument should have.
testarg_length <- function(arg, correct_length) {
  if (length(arg) != correct_length) {
    stop("'", substitute(arg), "' must have length ", correct_length)
  }
}

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
