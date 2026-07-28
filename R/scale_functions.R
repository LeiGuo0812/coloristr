#' Scale color for discrete palettes
#'
#' @param palette_name Character, name of the palette (can be either "name" or "source.name" format)
#' @param reverse Logical, whether to reverse the palette
#' @param ... Additional arguments passed to discrete_scale
#' @return A ggplot2 discrete color scale
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_discrete_cr("friendly")
#'
#' # Or use the full name
#' ggplot(mtcars, aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_discrete_cr("tidyplots.friendly")
scale_color_discrete_cr <- function(palette_name = "tidyplots.friendly", reverse = FALSE, ...) {
  discrete_scale_cr("colour", palette_name, reverse, list(...))
}

#' Scale fill for discrete palettes
#'
#' @param palette_name Character, name of the palette (can be either "name" or "source.name" format)
#' @param reverse Logical, whether to reverse the palette
#' @param ... Additional arguments passed to discrete_scale
#' @return A ggplot2 discrete fill scale
#' @export
scale_fill_discrete_cr <- function(palette_name = "tidyplots.friendly", reverse = FALSE, ...) {
  discrete_scale_cr("fill", palette_name, reverse, list(...))
}

#' Scale color for continuous palettes
#'
#' @param palette_name Character, name of the palette (can be either "name" or "source.name" format)
#' @param reverse Logical, whether to reverse the palette
#' @param ... Additional arguments passed to scale_color_gradientn
#' @return A ggplot2 continuous color scale
#' @export
scale_color_continuous_cr <- function(palette_name = "tidyplots.inferno", reverse = FALSE, ...) {
  pal <- get_continuous_palette_cr(palette_name, n = 256, reverse = reverse)
  gradient_scale_cr("colour", pal, list(...))
}

#' Scale fill for continuous palettes
#'
#' @param palette_name Character, name of the palette (can be either "name" or "source.name" format)
#' @param reverse Logical, whether to reverse the palette
#' @param ... Additional arguments passed to scale_fill_gradientn
#' @return A ggplot2 continuous fill scale
#' @export
scale_fill_continuous_cr <- function(palette_name = "tidyplots.inferno", reverse = FALSE, ...) {
  pal <- get_continuous_palette_cr(palette_name, n = 256, reverse = reverse)
  gradient_scale_cr("fill", pal, list(...))
}

#' Scale color for diverging palettes
#'
#' @param palette_name Character, name of the palette (can be either "name" or "source.name" format)
#' @param midpoint Numeric, the midpoint for diverging scales
#' @param reverse Logical, whether to reverse the palette
#' @param ... Additional arguments passed to scale_color_gradientn
#' @return A ggplot2 diverging color scale
#' @export
scale_color_diverging_cr <- function(palette_name = "tidyplots.spectral", midpoint = 0, reverse = FALSE, ...) {
  midpoint <- validate_number(midpoint, "midpoint")
  pal <- get_diverging_palette_cr(palette_name, n = 256, reverse = reverse)
  gradient_scale_cr("colour", pal, list(...), midpoint = midpoint)
}

#' Scale fill for diverging palettes
#'
#' @param palette_name Character, name of the palette (can be either "name" or "source.name" format)
#' @param midpoint Numeric, the midpoint for diverging scales
#' @param reverse Logical, whether to reverse the palette
#' @param ... Additional arguments passed to scale_fill_gradientn
#' @return A ggplot2 diverging fill scale
#' @export
scale_fill_diverging_cr <- function(palette_name = "tidyplots.spectral", midpoint = 0, reverse = FALSE, ...) {
  midpoint <- validate_number(midpoint, "midpoint")
  pal <- get_diverging_palette_cr(palette_name, n = 256, reverse = reverse)
  gradient_scale_cr("fill", pal, list(...), midpoint = midpoint)
}

discrete_scale_cr <- function(aesthetics, palette_name, reverse, dots) {
  reverse <- validate_flag(reverse, "reverse")
  resolved_name <- resolve_palette_info(
    palette_name,
    type = "discrete"
  )$palette_name
  palette <- function(n) {
    get_discrete_palette_cr(resolved_name, n = n, reverse = reverse)
  }

  args <- list(aesthetics = aesthetics, palette = palette)
  if (utils::packageVersion("ggplot2") < "3.5.0") {
    args <- append(
      args,
      list(scale_name = paste0("cr_", palette_name)),
      after = 1L
    )
  }
  do.call(ggplot2::discrete_scale, c(args, dots))
}

gradient_scale_cr <- function(aesthetics, palette, dots, midpoint = NULL) {
  dot_names <- names(dots)
  dot_names <- if (is.null(dot_names)) character() else dot_names
  n_colours <- sum(dot_names == "colours")
  n_colors <- sum(dot_names == "colors")

  if (n_colours + n_colors > 1L) {
    stop("Supply at most one of `colours` or `colors`", call. = FALSE)
  }

  override_name <- if (n_colours == 1L) {
    "colours"
  } else if (n_colors == 1L) {
    "colors"
  } else {
    NULL
  }

  if (!is.null(override_name)) {
    palette <- dots[[override_name]]
    dots[[override_name]] <- NULL
  }

  if (!is.null(midpoint) && !"rescaler" %in% dot_names) {
    transform <- if ("transform" %in% dot_names) {
      dots[["transform"]]
    } else if ("trans" %in% dot_names) {
      dots[["trans"]]
    } else {
      "identity"
    }
    dots$rescaler <- midpoint_rescaler(midpoint, transform)
  }

  scale_function <- if (aesthetics == "colour") {
    ggplot2::scale_color_gradientn
  } else {
    ggplot2::scale_fill_gradientn
  }
  do.call(scale_function, c(list(colours = palette), dots))
}

midpoint_rescaler <- function(midpoint, transform = "identity") {
  transformer <- scales::as.trans(transform)
  transformed_midpoint <- transformer$transform(midpoint)

  if (length(transformed_midpoint) != 1L ||
      !is.finite(transformed_midpoint)) {
    stop(
      "`midpoint` must remain finite after applying `transform`",
      call. = FALSE
    )
  }

  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(
      x,
      to = to,
      from = from,
      mid = transformed_midpoint
    )
  }
}
