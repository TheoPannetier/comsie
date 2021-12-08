#' Read simulation data
#'
#' Reads a `.csv` file as produced by [run_simulation()] and returns the parsed
#' table it contains.
#'
#' @param path_to_file character, path to the `.csv` file.
#' @param skip numeric, number of lines (of metadata) to skip. Passed to
#' [readr::read_csv()].
#'
#' @author Théo Pannetier
#' @export

read_comrad_tbl <- function(path_to_file, skip = 20) {

  comrad::testarg_char(path_to_file)
  if (!path_to_file %>% stringr::str_detect("\\.csv$")) {
    stop("'path_to_file' must be a .csv")
  }

  comrad_tbl <- readr::read_csv(
    path_to_file,
    skip = skip, # skip metadata
    col_types = readr::cols(
      readr::col_number(),    # t
      readr::col_number(),    # z
      readr::col_character(), # species
      readr::col_character(), # ancestral_species
      readr::col_character()  # root_species
    )
  )
  comrad_tbl
}
