#' Get discrete palette
#'
#' @param palette_name Character, name of the palette
#' @param n Integer, number of colors to return
#' @param reverse Logical, whether to reverse the palette
#' @return Character vector of colors
#' @export
#' @examples
#' get_discrete_palette("friendly", n = 4)
get_discrete_palette <- function(palette_name, n = NULL, reverse = FALSE) {
  pal <- discrete_palettes[[palette_name]]

  if(is.null(pal))
    stop("Palette not found")

  if(!is.null(n)) {
    if(n > length(pal)) {
      warning(sprintf("Requested %d colors but palette only has %d. Generating additional colors.",
                      n, length(pal)))

      base_colors <- evenly_sample_colors(pal, length(pal))
      n_additional <- n - length(pal)
      additional_colors <- generate_additional_colors(base_colors, n_additional)
      pal <- c(base_colors, additional_colors)
    } else {
      pal <- evenly_sample_colors(pal, n)
    }
  }

  if(reverse) pal <- rev(pal)
  return(pal)
}

#' Get continuous palette
#'
#' @param palette_name Character, name of the palette
#' @param n Integer, number of colors to return
#' @param reverse Logical, whether to reverse the palette
#' @return Character vector of colors
#' @export
#' @examples
#' get_continuous_palette("viridis", n = 100)
get_continuous_palette <- function(palette_name, n = 100, reverse = FALSE) {
  pal <- continuous_palettes[[palette_name]]

  if(is.null(pal))
    stop("Palette not found")

  if(reverse) pal <- rev(pal)
  grDevices::colorRampPalette(pal)(n)
}

#' Get diverging palette
#'
#' @param palette_name Character, name of the palette
#' @param n Integer, number of colors to return
#' @param reverse Logical, whether to reverse the palette
#' @return Character vector of colors
#' @export
#' @examples
#' get_diverging_palette("BuRd", n = 100)
get_diverging_palette <- function(palette_name, n = 100, reverse = FALSE) {
  pal <- diverging_palettes[[palette_name]]

  if(is.null(pal))
    stop("Palette not found")

  if(reverse) pal <- rev(pal)
  grDevices::colorRampPalette(pal)(n)
}
