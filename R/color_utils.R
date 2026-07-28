#' Evenly sample colors from a palette
#'
#' @param colors Character vector of colors
#' @param n Integer, number of colors to return
#' @return Character vector of selected colors
#' @keywords internal
evenly_sample_colors <- function(colors, n) {
  n <- validate_count(n, allow_zero = TRUE)

  if (n > length(colors)) {
    stop("Requested more colors than available in palette")
  }

  if (n == 0L) {
    return(colors[integer()])
  }

  if (n == length(colors)) {
    return(colors)
  }

  indices <- round(seq(1, length(colors), length.out = n))
  colors[indices]
}

#' Generate additional colors that are maximally distinct
#' @param existing_colors Character vector of existing hex colors
#' @param n_additional Integer, number of additional colors needed
#' @return Character vector of new hex colors
#' @keywords internal
generate_additional_colors <- function(existing_colors, n_additional) {
  n_additional <- validate_count(n_additional, allow_zero = TRUE,
                                 arg = "n_additional")

  if (n_additional == 0L) {
    return(character())
  }

  if (!is.character(existing_colors) || length(existing_colors) == 0L ||
      anyNA(existing_colors)) {
    stop("`existing_colors` must be a non-empty character vector",
         call. = FALSE)
  }

  hcl_colors <- farver::decode_colour(existing_colors, to = "hcl")

  mean_chroma <- mean(hcl_colors[, 2])
  mean_luminance <- mean(hcl_colors[, 3])
  existing_hues <- hcl_colors[, 1]

  if (!all(is.finite(c(mean_chroma, mean_luminance))) ||
      !any(is.finite(existing_hues))) {
    stop("`existing_colors` could not be converted to finite HCL values",
         call. = FALSE)
  }

  sorted_hues <- sort(unique(existing_hues[is.finite(existing_hues)]))
  gap_starts <- sorted_hues
  gap_ends <- c(sorted_hues[-1], sorted_hues[1] + 360)

  existing_rgb <- farver::decode_colour(existing_colors, to = "rgb")
  seen_colors <- toupper(
    farver::encode_colour(existing_rgb, from = "rgb")
  )
  new_colors <- character()
  max_attempts <- max(1000, as.double(n_additional) * 100)
  attempts <- 0

  while (length(new_colors) < n_additional && attempts < max_attempts) {
    batch_size <- as.integer(
      min(
        max(100, as.double(n_additional) * 2),
        100000,
        max_attempts - attempts
      )
    )
    candidate_hues <- numeric(batch_size)

    for (i in seq_len(batch_size)) {
      max_gap_idx <- which.max(gap_ends - gap_starts)
      gap_start <- gap_starts[max_gap_idx]
      gap_end <- gap_ends[max_gap_idx]
      gap_midpoint <- gap_start + (gap_end - gap_start) / 2

      candidate_hues[i] <- gap_midpoint %% 360
      gap_ends[max_gap_idx] <- gap_midpoint
      gap_starts <- c(gap_starts, gap_midpoint)
      gap_ends <- c(gap_ends, gap_end)
    }
    attempts <- attempts + batch_size

    candidate_hcl <- matrix(
      c(
        candidate_hues,
        rep(mean_chroma, batch_size),
        rep(mean_luminance, batch_size)
      ),
      ncol = 3
    )
    candidates <- toupper(
      farver::encode_colour(candidate_hcl, from = "hcl")
    )
    keep <- !is.na(candidates) &
      !candidates %in% c(seen_colors, new_colors) &
      !duplicated(candidates)
    candidates <- candidates[keep]

    if (length(candidates) > 0L) {
      n_needed <- n_additional - length(new_colors)
      n_take <- min(length(candidates), n_needed)
      new_colors <- c(new_colors, candidates[seq_len(n_take)])
    }
  }

  if (length(new_colors) < n_additional) {
    stop(
      sprintf(
        "Could only generate %d unique additional colors out of %d requested",
        length(new_colors), n_additional
      ),
      call. = FALSE
    )
  }

  unname(new_colors)
}

validate_scalar_character <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("`%s` must be a non-empty character scalar", arg),
         call. = FALSE)
  }
  x
}

validate_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be TRUE or FALSE", arg), call. = FALSE)
  }
  x
}

validate_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    stop(sprintf("`%s` must be a finite numeric scalar", arg),
         call. = FALSE)
  }
  x
}

validate_count <- function(x, allow_zero = TRUE, arg = "n") {
  lower_bound <- if (allow_zero) 0 else 1

  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x != floor(x) || x < lower_bound || x > .Machine$integer.max) {
    qualifier <- if (allow_zero) "non-negative" else "positive"
    stop(
      sprintf("`%s` must be a finite %s integer scalar", arg, qualifier),
      call. = FALSE
    )
  }

  as.integer(x)
}

validate_optional_choice <- function(x, choices, arg) {
  if (is.null(x)) {
    return(NULL)
  }

  x <- validate_scalar_character(x, arg)
  if (!x %in% choices) {
    stop(
      sprintf("`%s` must be one of: %s",
              arg, paste(choices, collapse = ", ")),
      call. = FALSE
    )
  }
  x
}
