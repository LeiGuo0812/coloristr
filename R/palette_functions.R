#' Get discrete palette
#'
#' @param palette_name Character, name of the palette (can be either "name" or "source.name" format)
#' @param n Integer, number of colors to return
#' @param reverse Logical, whether to reverse the palette
#' @return Character vector of colors
#' @export
#' @examples
#' get_discrete_palette_cr("friendly", n = 4)
#' get_discrete_palette_cr("tidyplots.friendly", n = 4)
get_discrete_palette_cr <- function(palette_name, n = NULL, reverse = FALSE) {
  # Handle both naming conventions
  if (!grepl("\\.", palette_name)) {
    # If no source prefix, search by name
    matched_row <- palette_info[palette_info$name == palette_name & palette_info$type == "discrete", ]
    if (nrow(matched_row) > 0) {
      palette_name <- matched_row$palette_name[1]
    }
  }

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
#' @param palette_name Character, name of the palette (can be either "name" or "source.name" format)
#' @param n Integer, number of colors to return
#' @param reverse Logical, whether to reverse the palette
#' @return Character vector of colors
#' @export
#' @examples
#' get_continuous_palette_cr("viridis", n = 100)
#' get_continuous_palette_cr("tidyplots.viridis", n = 100)
get_continuous_palette_cr <- function(palette_name, n = 100, reverse = FALSE) {
  if (!grepl("\\.", palette_name)) {
    matched_row <- palette_info[palette_info$name == palette_name & palette_info$type == "continuous", ]
    if (nrow(matched_row) > 0) {
      palette_name <- matched_row$palette_name[1]
    }
  }

  pal <- continuous_palettes[[palette_name]]

  if(is.null(pal))
    stop("Palette not found")

  if(reverse) pal <- rev(pal)
  grDevices::colorRampPalette(pal)(n)
}

#' Get diverging palette
#'
#' @param palette_name Character, name of the palette (can be either "name" or "source.name" format)
#' @param n Integer, number of colors to return
#' @param reverse Logical, whether to reverse the palette
#' @return Character vector of colors
#' @export
#' @examples
#' get_diverging_palette_cr("spectral", n = 100)
#' get_diverging_palette_cr("tidyplots.spectral", n = 100)
get_diverging_palette_cr <- function(palette_name, n = 100, reverse = FALSE) {
  if (!grepl("\\.", palette_name)) {
    matched_row <- palette_info[palette_info$name == palette_name & palette_info$type == "diverging", ]
    if (nrow(matched_row) > 0) {
      palette_name <- matched_row$palette_name[1]
    }
  }

  pal <- diverging_palettes[[palette_name]]

  if(is.null(pal))
    stop("Palette not found")

  if(reverse) pal <- rev(pal)
  grDevices::colorRampPalette(pal)(n)
}
