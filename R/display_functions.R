#' Display color palette
#'
#' @param palette_name Character, name of the palette (can be either "name" or "source.name" format)
#' @param n Integer, number of colors to display (only for continuous and diverging palettes)
#' @param type Character, optional palette type when source and name are not unique
#' @return Invisibly, the character vector of displayed colors
#' @export
#'
#' @examples
#' # Show a discrete palette using either naming convention
#' display_palette("tidyplots.friendly")
#' display_palette("friendly")
#'
#' # Show a continuous palette with 10 colors
#' display_palette("tidyplots.inferno", n = 10, type = "continuous")
#'
#' # Show a diverging palette
#' display_palette("tidyplots.spectral", type = "diverging")
display_palette <- function(palette_name, n = NULL, type = NULL) {
  pal_info <- resolve_palette_info(palette_name, type)

  colors <- if (pal_info$type == "discrete") {
    unlist(pal_info$colors)
  } else {
    if (is.null(n)) {
      n <- 10L
    }
    n <- validate_count(n, allow_zero = FALSE)
    grDevices::colorRampPalette(unlist(pal_info$colors))(n)
  }

  scales::show_col(colors)
  invisible(colors)
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
#' list_palettes(source = "tidyplots")
#'
#' # List discrete palettes from tidyplots
#' list_palettes(source = "tidyplots", type = "discrete")
list_palettes <- function(type = NULL, source = NULL) {
  type <- validate_optional_choice(
    type,
    c("discrete", "continuous", "diverging"),
    "type"
  )
  source <- validate_optional_choice(
    source,
    sort(unique(palette_info$source)),
    "source"
  )

  result <- palette_info[, c("palette_name", "source", "name", "type", "n_colors")]

  if (!is.null(type)) {
    result <- result[result$type == type, , drop = FALSE]
  }

  if (!is.null(source)) {
    result <- result[result$source == source, , drop = FALSE]
  }

  rownames(result) <- NULL
  result
}

#' Display all available color palettes in a grid layout
#'
#' @param type Character, optional filter for palette type. One of "discrete", "continuous", or "diverging"
#' @param source Character, optional filter for palette source
#' @param name Character, optional filter for palette name
#' @param n Integer, number of colors to display for continuous and diverging palettes (default: 7)
#' @return Invisibly, the filtered palette information
#' @export
#'
#' @examples
#' # Display all palettes
#' display_all_palettes()
#'
#' # Display only discrete palettes
#' display_all_palettes(type = "discrete")
#'
#' # Display palettes from a specific source
#' display_all_palettes(source = "tidyplots")
#'
#' # Display palettes with specific name
#' display_all_palettes(name = "friendly")
#'
#' # Combine filters
#' display_all_palettes(source = "tidyplots", name = "friendly")
#' display_all_palettes(source = "tidyplots", type = "discrete")
display_all_palettes <- function(type = NULL, source = NULL, name = NULL, n = 7) {
  type <- validate_optional_choice(
    type,
    c("discrete", "continuous", "diverging"),
    "type"
  )
  source <- validate_optional_choice(
    source,
    sort(unique(palette_info$source)),
    "source"
  )
  name <- validate_optional_choice(
    name,
    sort(unique(palette_info$name)),
    "name"
  )
  n <- validate_count(n, allow_zero = FALSE)

  pal_info <- palette_info[, c("source", "name", "type", "n_colors", "colors")]

  if (!is.null(type)) {
    pal_info <- pal_info[pal_info$type == type, , drop = FALSE]
  }
  if (!is.null(source)) {
    pal_info <- pal_info[pal_info$source == source, , drop = FALSE]
  }
  if (!is.null(name)) {
    pal_info <- pal_info[pal_info$name == name, , drop = FALSE]
  }

  if (nrow(pal_info) == 0) {
    stop(sprintf(
      "No palettes found with:\n  type = %s\n  source = %s\n  name = %s",
      if (is.null(type)) "any" else type,
      if (is.null(source)) "any" else source,
      if (is.null(name)) "any" else name
    ), call. = FALSE)
  }

  n_plots <- nrow(pal_info)
  n_cols <- min(5, n_plots)
  n_rows <- ceiling(n_plots / n_cols)

  grid::grid.newpage()

  for (i in seq_len(n_plots)) {
    row <- ceiling(i / n_cols)
    col <- ((i - 1) %% n_cols) + 1

    vp <- grid::viewport(
      x = (col - 0.5) / n_cols,
      y = 1 - (row - 0.5) / n_rows,
      width = 1 / n_cols,
      height = 1 / n_rows
    )
    grid::pushViewport(vp)

    colors <- if (pal_info$type[i] == "discrete") {
      unlist(pal_info$colors[i])
    } else {
      grDevices::colorRampPalette(unlist(pal_info$colors[i]))(n)
    }

    if (pal_info$type[i] == "discrete") {
      n_colors <- length(colors)
      grid::grid.rect(
        x = (seq_len(n_colors) - 0.5) / n_colors * 0.8 + 0.1,
        width = 0.8 / n_colors * 0.9,
        height = 0.3,
        gp = grid::gpar(fill = colors, col = NA)
      )
    } else {
      grid::grid.raster(matrix(colors, nrow = 1), width = 0.8, height = 0.3)
    }

    display_text <- if(pal_info$type[i] == "discrete") {
      paste0(
        pal_info$source[i], ".", pal_info$name[i],
        "\n(", pal_info$type[i], ", n=", pal_info$n_colors[i], ")"
      )
    } else {
      paste0(
        pal_info$source[i], ".", pal_info$name[i],
        "\n(", pal_info$type[i], ")"
      )
    }

    grid::grid.text(
      display_text,
      y = 0.2,
      gp = grid::gpar(fontsize = 8)
    )

    grid::upViewport()
  }

  rownames(pal_info) <- NULL
  invisible(pal_info)
}
