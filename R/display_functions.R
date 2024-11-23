#' Display color palette
#'
#' @param palette_name Character, name of the palette
#' @param n Integer, number of colors to display (only for continuous and diverging palettes)
#' @importFrom scales show_col
#' @export
#'
#' @examples
#' # Show a discrete palette
#' display_palette("friendly")
#'
#' # Show a continuous palette with 10 colors
#' display_palette("viridis", n = 10)
display_palette <- function(palette_name, n = NULL) {
  # get palette information
  pal_info <- palette_info[palette_info$palette_name == palette_name, ]

  if(nrow(pal_info) == 0) {
    stop("Palette not found")
  }

  # chose color based on palette type
  colors <- if(pal_info$type == "discrete") {
    unlist(pal_info$colors)
  } else {
    n <- n %||% 10  # if n is NULL，use defualt 10
    # using colorRampPalette to generate colors
    grDevices::colorRampPalette(unlist(pal_info$colors))(n)
  }

  # show colors
  scales::show_col(colors)
}

# Helper function for NULL coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x
