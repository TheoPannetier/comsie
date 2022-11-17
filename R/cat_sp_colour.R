#' Cat the name of a species, coloured by its hex colour code
#'
#' @param sp_name character, a 6-digit hex code for a colour that also serves as
#' name for the species
#' @param bg logical, should the colour apply to the background?
#'
#' @export
#'
cat_sp_colour <- function(sp_name, bg = FALSE) {
  col_func <- crayon::make_style(sp_name, bg = bg)
  cat(col_func(sp_name))
}
