#' Scale color for discrete palettes
#'
#' @param palette_name Character, name of the palette (can be either "name" or "source.name" format)
#' @param reverse Logical, whether to reverse the palette
#' @param ... Additional arguments passed to discrete_scale
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_discrete_pal("friendly")
#'
#' # Or use the full name
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_discrete_pal("tidyplots.friendly")
scale_color_discrete_pal <- function(palette_name = "tidyplots.friendly", reverse = FALSE, ...) {
  tryCatch({
    ggplot2::discrete_scale(
      "colour", paste0("pal_", palette_name),
      function(n) get_discrete_palette(palette_name, n, reverse),
      ...
    )
  }, error = function(e) {
    if(grepl("Palette not found", e$message)) {
      available_discrete <- paste(names(discrete_palettes), collapse = ", ")
      stop(sprintf("Palette '%s' not found.\nAvailable discrete palettes are: %s",
                   palette_name, available_discrete), call. = FALSE)
    } else {
      stop(e)
    }
  })
}

#' Scale fill for discrete palettes
#'
#' @param palette_name Character, name of the palette (can be either "name" or "source.name" format)
#' @param reverse Logical, whether to reverse the palette
#' @param ... Additional arguments passed to discrete_scale
#' @export
scale_fill_discrete_pal <- function(palette_name = "tidyplots.friendly", reverse = FALSE, ...) {
  tryCatch({
    ggplot2::discrete_scale(
      "fill", paste0("pal_", palette_name),
      function(n) get_discrete_palette(palette_name, n, reverse),
      ...
    )
  }, error = function(e) {
    if(grepl("Palette not found", e$message)) {
      available_discrete <- paste(names(discrete_palettes), collapse = ", ")
      stop(sprintf("Palette '%s' not found.\nAvailable discrete palettes are: %s",
                   palette_name, available_discrete), call. = FALSE)
    } else {
      stop(e)
    }
  })
}

#' Scale color for continuous palettes
#'
#' @param palette_name Character, name of the palette (can be either "name" or "source.name" format)
#' @param reverse Logical, whether to reverse the palette
#' @param ... Additional arguments passed to scale_color_gradientn
#' @export
scale_color_continuous_pal <- function(palette_name = "tidyplots.inferno", reverse = FALSE, ...) {
  tryCatch({
    pal <- get_continuous_palette(palette_name, n = 256, reverse = reverse)
    ggplot2::scale_color_gradientn(colors = pal, ...)
  }, error = function(e) {
    if(grepl("Palette not found", e$message)) {
      available_continuous <- paste(names(continuous_palettes), collapse = ", ")
      stop(sprintf("Palette '%s' not found.\nAvailable continuous palettes are: %s",
                   palette_name, available_continuous), call. = FALSE)
    } else {
      stop(e)
    }
  })
}

#' Scale fill for continuous palettes
#'
#' @param palette_name Character, name of the palette (can be either "name" or "source.name" format)
#' @param reverse Logical, whether to reverse the palette
#' @param ... Additional arguments passed to scale_fill_gradientn
#' @export
scale_fill_continuous_pal <- function(palette_name = "tidyplots.inferno", reverse = FALSE, ...) {
  tryCatch({
    pal <- get_continuous_palette(palette_name, n = 256, reverse = reverse)
    ggplot2::scale_fill_gradientn(colors = pal, ...)
  }, error = function(e) {
    if(grepl("Palette not found", e$message)) {
      available_continuous <- paste(names(continuous_palettes), collapse = ", ")
      stop(sprintf("Palette '%s' not found.\nAvailable continuous palettes are: %s",
                   palette_name, available_continuous), call. = FALSE)
    } else {
      stop(e)
    }
  })
}

#' Scale color for diverging palettes
#'
#' @param palette_name Character, name of the palette (can be either "name" or "source.name" format)
#' @param midpoint Numeric, the midpoint for diverging scales
#' @param reverse Logical, whether to reverse the palette
#' @param ... Additional arguments passed to scale_color_gradientn
#' @export
scale_color_diverging_pal <- function(palette_name = "tidyplots.spectral", midpoint = 0, reverse = FALSE, ...) {
  tryCatch({
    pal <- get_diverging_palette(palette_name, n = 256, reverse = reverse)
    ggplot2::scale_color_gradientn(
      colors = pal,
      values = scales::rescale(c(0, midpoint, 1)),
      ...
    )
  }, error = function(e) {
    if(grepl("Palette not found", e$message)) {
      available_diverging <- paste(names(diverging_palettes), collapse = ", ")
      stop(sprintf("Palette '%s' not found.\nAvailable diverging palettes are: %s",
                   palette_name, available_diverging), call. = FALSE)
    } else {
      stop(e)
    }
  })
}

#' Scale fill for diverging palettes
#'
#' @param palette_name Character, name of the palette (can be either "name" or "source.name" format)
#' @param midpoint Numeric, the midpoint for diverging scales
#' @param reverse Logical, whether to reverse the palette
#' @param ... Additional arguments passed to scale_fill_gradientn
#' @export
scale_fill_diverging_pal <- function(palette_name = "tidyplots.spectral", midpoint = 0, reverse = FALSE, ...) {
  tryCatch({
    pal <- get_diverging_palette(palette_name, n = 256, reverse = reverse)
    ggplot2::scale_fill_gradientn(
      colors = pal,
      values = scales::rescale(c(0, midpoint, 1)),
      ...
    )
  }, error = function(e) {
    if(grepl("Palette not found", e$message)) {
      available_diverging <- paste(names(diverging_palettes), collapse = ", ")
      stop(sprintf("Palette '%s' not found.\nAvailable diverging palettes are: %s",
                   palette_name, available_diverging), call. = FALSE)
    } else {
      stop(e)
    }
  })
}
