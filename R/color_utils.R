#' Evenly sample colors from a palette
#'
#' @param colors Character vector of colors
#' @param n Integer, number of colors to return
#' @return Character vector of selected colors
#' @keywords internal
evenly_sample_colors <- function(colors, n) {
  if (n > length(colors)) {
    stop("Requested more colors than available in palette")
  }

  if (n == length(colors)) {
    return(colors)
  }

  if (n == 1) {
    return(colors[1])
  }

  if (n == 2) {
    return(c(colors[1], colors[length(colors)]))
  }

  selected <- c(colors[1], colors[length(colors)])  # the first and the last color

  if (n > 2) {
    n_middle <- n - 2
    middle_colors <- colors[-c(1, length(colors))]
    if (n_middle == 1) {
      middle_idx <- ceiling(length(middle_colors)/2)
      selected <- c(selected[1], middle_colors[middle_idx], selected[2])
    } else {
      indices <- round(seq(1, length(middle_colors), length.out = n_middle))
      selected <- c(selected[1], middle_colors[indices], selected[2])
    }
  }

  return(selected)
}

#' Generate additional colors that are maximally distinct
#' @param existing_colors Character vector of existing hex colors
#' @param n_additional Integer, number of additional colors needed
#' @return Character vector of new hex colors
#' @keywords internal
generate_additional_colors <- function(existing_colors, n_additional) {
  hcl_colors <- farver::decode_colour(existing_colors, to = "hcl")

  mean_chroma <- mean(hcl_colors[,2])
  mean_luminance <- mean(hcl_colors[,3])
  existing_hues <- hcl_colors[,1]

  sorted_hues <- sort(existing_hues)
  hue_gaps <- diff(c(sorted_hues, sorted_hues[1] + 360))

  new_hues <- numeric(n_additional)
  for(i in 1:n_additional) {
    max_gap_idx <- which.max(hue_gaps)
    gap_start <- sorted_hues[max_gap_idx]
    gap_end <- if(max_gap_idx == length(sorted_hues)) {
      sorted_hues[1] + 360
    } else {
      sorted_hues[max_gap_idx + 1]
    }

    new_hue <- gap_start + (gap_end - gap_start) / 2
    if(new_hue >= 360) new_hue <- new_hue - 360

    new_hues[i] <- new_hue
    sorted_hues <- sort(c(sorted_hues, new_hue))
    hue_gaps <- diff(c(sorted_hues, sorted_hues[1] + 360))
  }

  new_colors_hcl <- matrix(
    c(new_hues,
      rep(mean_chroma, n_additional),
      rep(mean_luminance, n_additional)),
    ncol = 3
  )

  farver::encode_colour(new_colors_hcl, from = "hcl")
}
