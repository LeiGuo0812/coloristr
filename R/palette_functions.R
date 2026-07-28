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
  reverse <- validate_flag(reverse, "reverse")
  palette_name <- resolve_palette_info(
    palette_name,
    type = "discrete"
  )$palette_name
  pal <- discrete_palettes[[palette_name]]

  if (!is.null(n)) {
    n <- validate_count(n, allow_zero = TRUE)
    if (n > length(pal)) {
      warning(
        sprintf(
          "Requested %d colors but palette only has %d. Generating additional colors.",
          n, length(pal)
        ),
        call. = FALSE
      )

      base_colors <- evenly_sample_colors(pal, length(pal))
      n_additional <- n - length(pal)
      additional_colors <- generate_additional_colors(base_colors, n_additional)
      pal <- c(base_colors, additional_colors)
    } else {
      pal <- evenly_sample_colors(pal, n)
    }
  }

  if (reverse) {
    pal <- rev(pal)
  }
  unname(pal)
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
  n <- validate_count(n, allow_zero = TRUE)
  reverse <- validate_flag(reverse, "reverse")
  palette_name <- resolve_palette_info(
    palette_name,
    type = "continuous"
  )$palette_name
  pal <- continuous_palettes[[palette_name]]

  if (n == 0L) {
    return(character())
  }

  if (reverse) {
    pal <- rev(pal)
  }
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
  n <- validate_count(n, allow_zero = TRUE)
  reverse <- validate_flag(reverse, "reverse")
  palette_name <- resolve_palette_info(
    palette_name,
    type = "diverging"
  )$palette_name
  pal <- diverging_palettes[[palette_name]]

  if (n == 0L) {
    return(character())
  }

  if (reverse) {
    pal <- rev(pal)
  }
  grDevices::colorRampPalette(pal)(n)
}

resolve_palette_info <- function(palette_name, type = NULL) {
  palette_name <- validate_scalar_character(palette_name, "palette_name")
  type <- validate_optional_choice(
    type,
    c("discrete", "continuous", "diverging"),
    "type"
  )

  if (grepl("\\.", palette_name)) {
    matches <- palette_info$palette_name == palette_name
  } else {
    matches <- palette_info$name == palette_name
  }

  if (!is.null(type)) {
    matches <- matches & palette_info$type == type
  }

  candidates <- palette_info[matches, , drop = FALSE]

  if (nrow(candidates) == 0L) {
    type_text <- if (is.null(type)) "" else sprintf(" with type '%s'", type)
    stop(
      sprintf("Palette '%s'%s not found", palette_name, type_text),
      call. = FALSE
    )
  }

  if (nrow(candidates) > 1L) {
    available_types <- paste(sort(unique(candidates$type)), collapse = ", ")
    stop(
      sprintf(
        "Palette '%s' is ambiguous; specify `type` as one of: %s",
        palette_name, available_types
      ),
      call. = FALSE
    )
  }

  candidates
}
