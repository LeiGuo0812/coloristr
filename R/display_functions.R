#' Display color palette
#'
#' @param palette_name Character, name of the palette (can be either "name" or "source.name" format)
#' @param n Integer, number of colors to display (only for continuous and diverging palettes)
#' @importFrom scales show_col
#' @export
#'
#' @examples
#' # Show a discrete palette using either naming convention
#' display_palette("friendly")
#' display_palette("tidyplots.friendly")
#'
#' # Show a continuous palette with 10 colors
#' display_palette("viridis", n = 10)
#' display_palette("tidyplots.viridis", n = 10)
display_palette <- function(palette_name, n = NULL) {
  # Handle both naming conventions
  if (!grepl("\\.", palette_name)) {
    matched_row <- palette_info[palette_info$name == palette_name, ]
    if (nrow(matched_row) > 0) {
      palette_name <- matched_row$palette_name[1]
    }
  }

  # get palette information
  pal_info <- palette_info[palette_info$palette_name == palette_name, ]

  if(nrow(pal_info) == 0) {
    stop("Palette not found")
  }

  # choose color based on palette type
  colors <- if(pal_info$type == "discrete") {
    unlist(pal_info$colors)
  } else {
    n <- n %||% 10  # if n is NULL，use default 10
    # using colorRampPalette to generate colors
    grDevices::colorRampPalette(unlist(pal_info$colors))(n)
  }

  # show colors
  scales::show_col(colors)
}

#' List all available palettes
#'
#' @param type Character, optional filter for palette type ("discrete", "continuous", "diverging")
#' @param source Character, optional filter for palette source
#' @return A data frame containing palette information
#' @export
#'
#' @examples
#' # List all palettes
#' list_palettes()
#'
#' # List only discrete palettes
#' list_palettes(type = "discrete")
#'
#' # List palettes from a specific source
#' list_palettes(source = "viridis")
list_palettes <- function(type = NULL, source = NULL) {
  result <- palette_info[, c("palette_name", "source", "name", "type", "n_colors")]

  if (!is.null(type)) {
    result <- result[result$type == type, ]
  }

  if (!is.null(source)) {
    result <- result[result$source == source, ]
  }

  return(result)
}

# Helper function for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x
