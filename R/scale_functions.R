#' Scale color for discrete palettes
#'
#' @param palette_name Character, name of the palette
#' @param reverse Logical, whether to reverse the palette
#' @param ... Additional arguments passed to discrete_scale
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_discrete_pal("friendly")
scale_color_discrete_pal <- function(palette_name = 'friendly', reverse = FALSE, ...) {
  ggplot2::discrete_scale(
    "colour", paste0("pal_", palette_name),
    function(n) get_discrete_palette(palette_name, n, reverse),
    ...
  )
}

#' Scale fill for discrete palettes
#'
#' @param palette_name Character, name of the palette
#' @param reverse Logical, whether to reverse the palette
#' @param ... Additional arguments passed to discrete_scale
#' @export
scale_fill_discrete_pal <- function(palette_name = 'friendly', reverse = FALSE, ...) {
  ggplot2::discrete_scale(
    "fill", paste0("pal_", palette_name),
    function(n) get_discrete_palette(palette_name, n, reverse),
    ...
  )
}

#' Scale color for continuous palettes
#'
#' @param palette_name Character, name of the palette
#' @param reverse Logical, whether to reverse the palette
#' @param ... Additional arguments passed to scale_color_gradientn
#' @export
scale_color_continuous_pal <- function(palette_name = 'inferno', reverse = FALSE, ...) {
  pal <- get_continuous_palette(palette_name, n = 256, reverse = reverse)
  ggplot2::scale_color_gradientn(colors = pal, ...)
}

#' Scale fill for continuous palettes
#'
#' @param palette_name Character, name of the palette
#' @param reverse Logical, whether to reverse the palette
#' @param ... Additional arguments passed to scale_fill_gradientn
#' @export
scale_fill_continuous_pal <- function(palette_name = 'inferno', reverse = FALSE, ...) {
  pal <- get_continuous_palette(palette_name, n = 256, reverse = reverse)
  ggplot2::scale_fill_gradientn(colors = pal, ...)
}

#' Scale color for diverging palettes
#'
#' @param palette_name Character, name of the palette
#' @param midpoint Numeric, the midpoint for diverging scales
#' @param reverse Logical, whether to reverse the palette
#' @param ... Additional arguments passed to scale_color_gradientn
#' @export
scale_color_diverging_pal <- function(palette_name = 'spectral', midpoint = 0, reverse = FALSE, ...) {
  pal <- get_diverging_palette(palette_name, n = 256, reverse = reverse)
  ggplot2::scale_color_gradientn(
    colors = pal,
    values = scales::rescale(c(0, midpoint, 1)),
    ...
  )
}

#' Scale fill for diverging palettes
#'
#' @param palette_name Character, name of the palette
#' @param midpoint Numeric, the midpoint for diverging scales
#' @param reverse Logical, whether to reverse the palette
#' @param ... Additional arguments passed to scale_fill_gradientn
#' @export
scale_fill_diverging_pal <- function(palette_name = 'spectral', midpoint = 0, reverse = FALSE, ...) {
  pal <- get_diverging_palette(palette_name, n = 256, reverse = reverse)
  ggplot2::scale_fill_gradientn(
    colors = pal,
    values = scales::rescale(c(0, midpoint, 1)),
    ...
  )
}
