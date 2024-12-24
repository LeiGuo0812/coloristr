#' Display color palette
#'
#' @param palette_name Character, name of the palette (can be either "name" or "source.name" format)
#' @param n Integer, number of colors to display (only for continuous and diverging palettes)
#' @param type Character, optional palette type when source and name are not unique
#' @importFrom scales show_col
#' @export
#'
#' @examples
#' # Show a discrete palette using either naming convention
#' display_palette("tidyplots.friendly")
#' display_palette("friendly")
#'
#' # Show a continuous palette with 10 colors
#' display_palette("tidyplots.friendly", n = 10, type = "continuous")
#'
#' # Show a diverging palette
#' display_palette("tidyplots.friendly", type = "diverging")
display_palette <- function(palette_name, n = NULL, type = NULL) {
  # Handle both naming conventions
  if (!grepl("\\.", palette_name)) {
    matched_row <- palette_info[palette_info$name == palette_name, ]
    if (!is.null(type)) {
      matched_row <- matched_row[matched_row$type == type, ]
    }
    if (nrow(matched_row) > 0) {
      palette_name <- matched_row$palette_name[1]
    } else if (!is.null(type)) {
      stop(sprintf("Palette '%s' with type '%s' not found", palette_name, type))
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
#' list_palettes(source = "tidyplots")
#'
#' # List discrete palettes from tidyplots
#' list_palettes(source = "tidyplots", type = "discrete")
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

#' Display all available color palettes in a grid layout
#'
#' @param type Character, optional filter for palette type. One of "discrete", "continuous", or "diverging"
#' @param source Character, optional filter for palette source
#' @param name Character, optional filter for palette name
#' @param n Integer, number of colors to display for continuous and diverging palettes (default: 7)
#' @return Creates a grid display of color palettes in the current graphics device
#' @importFrom grid grid.newpage pushViewport viewport grid.text
#' @importFrom grDevices colorRampPalette
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
  # Validate type parameter
  if (!is.null(type) && !type %in% c("discrete", "continuous", "diverging")) {
    stop('type must be one of "discrete", "continuous", or "diverging"')
  }

  # Validate source parameter
  valid_sources <- unique(palette_info$source)
  if (!is.null(source) && !source %in% valid_sources) {
    stop(sprintf("source must be one of: %s", 
                paste(valid_sources, collapse = ", ")))
  }

  # Validate name parameter
  valid_names <- unique(palette_info$name)
  if (!is.null(name) && !name %in% valid_names) {
    stop(sprintf("name must be one of: %s", 
                paste(valid_names, collapse = ", ")))
  }

  # Get filtered palette information
  pal_info <- palette_info[, c("source", "name", "type", "n_colors", "colors")]
  
  if (!is.null(type)) {
    pal_info <- pal_info[pal_info$type == type, ]
  }
  if (!is.null(source)) {
    pal_info <- pal_info[pal_info$source == source, ]
  }
  if (!is.null(name)) {
    pal_info <- pal_info[pal_info$name == name, ]
  }
  
  if (nrow(pal_info) == 0) {
    stop(sprintf(
      "No palettes found with:\n  type = %s\n  source = %s\n  name = %s",
      if(is.null(type)) "any" else type,
      if(is.null(source)) "any" else source,
      if(is.null(name)) "any" else name
    ))
  }
  
  # Create a list to store all color plots
  plot_list <- list()
  
  # Generate color displays for each palette
  for (i in seq_len(nrow(pal_info))) {
    palette_name <- pal_info$palette_name[i]
    pal_type <- pal_info$type[i]
    
    # Get colors based on palette type
    colors <- if(pal_type == "discrete") {
      unlist(pal_info$colors[i])
    } else {
      grDevices::colorRampPalette(
        unlist(pal_info$colors[i])
      )(n)
    }
    
    # Create color rectangle plot
    plot_list[[i]] <- {
      p <- if(pal_type == "discrete") {
        colors  # For discrete palettes, store color vector directly
      } else {
        matrix(colors, nrow = 1)  # For continuous palettes, create matrix
      }
      list(
        p = p,
        name = paste0(
          pal_info$source[i], ".", pal_info$name[i],
          "\n(", pal_info$type[i], ")"
        )
      )
    }
  }
  
  # Calculate grid dimensions
  n_plots <- length(plot_list)
  n_cols <- min(5, n_plots)
  n_rows <- ceiling(n_plots / n_cols)
  
  # Create the display
  grid::grid.newpage()
  
  # Layout the plots in a grid
  for (i in seq_len(n_plots)) {
    row <- ceiling(i / n_cols)
    col <- ((i - 1) %% n_cols) + 1
    
    vp <- grid::viewport(
      x = (col - 0.5) / n_cols,
      y = 1 - (row - 0.5) / n_rows,
      width = 1/n_cols,
      height = 1/n_rows
    )
    grid::pushViewport(vp)
    
    # Plot colors
    if (pal_info$type[i] == "discrete") {
      # For discrete palettes, draw individual color blocks
      colors <- unlist(pal_info$colors[i])
      n_colors <- length(colors)
      
      for (j in seq_len(n_colors)) {
        grid::grid.rect(
          x = (j - 0.5) / n_colors * 0.8 + 0.1,  # 0.8 is total width, 0.1 is left margin
          width = 0.8 / n_colors * 0.9,  # 0.9 to leave small gaps between blocks
          height = 0.3,
          gp = grid::gpar(fill = colors[j], col = NA)  # col = NA removes border
        )
      }
    } else {
      # For continuous palettes, use original method
      grid::grid.raster(plot_list[[i]]$p, width = 0.8, height = 0.3)
    }
    grid::grid.text(
      plot_list[[i]]$name,
      y = 0.2,
      gp = grid::gpar(fontsize = 8)
    )
    
    grid::upViewport()
  }
}
