test_that("discrete palettes are sampled evenly across the full range", {
  apple <- coloristr:::discrete_palettes[["tidyplots.apple"]]

  expect_identical(
    get_discrete_palette_cr("tidyplots.apple", n = 4),
    apple[c(1, 3, 5, 7)]
  )
  expect_identical(
    get_discrete_palette_cr("apple", n = 2),
    apple[c(1, 7)]
  )
})

test_that("palette getters validate counts and flags consistently", {
  expect_identical(
    get_discrete_palette_cr("friendly", n = 0),
    character()
  )
  expect_identical(
    get_continuous_palette_cr("viridis", n = 0),
    character()
  )
  expect_identical(
    get_diverging_palette_cr("spectral", n = 0),
    character()
  )

  invalid_counts <- list(-1, 1.5, NA_real_, Inf, c(1, 2))
  for (n in invalid_counts) {
    expect_error(
      get_discrete_palette_cr("friendly", n = n),
      "finite non-negative integer scalar",
      fixed = TRUE
    )
    expect_error(
      get_continuous_palette_cr("viridis", n = n),
      "finite non-negative integer scalar",
      fixed = TRUE
    )
  }

  expect_error(
    get_discrete_palette_cr("friendly", reverse = NA),
    "`reverse` must be TRUE or FALSE",
    fixed = TRUE
  )
  expect_error(
    get_continuous_palette_cr(c("viridis", "magma")),
    "`palette_name` must be a non-empty character scalar",
    fixed = TRUE
  )
})

test_that("reverse preserves the selected colors and reverses their order", {
  forward <- get_discrete_palette_cr("friendly", n = 4)
  reverse <- get_discrete_palette_cr("friendly", n = 4, reverse = TRUE)

  expect_identical(reverse, rev(forward))
})

test_that("generated additional colors remain unique", {
  expect_identical(
    coloristr:::generate_additional_colors(
      coloristr:::discrete_palettes[["tidyplots.friendly"]],
      0
    ),
    character()
  )

  expect_warning(
    colors <- get_discrete_palette_cr("nature.vivid_light", n = 61),
    "Generating additional colors",
    fixed = TRUE
  )
  expect_length(colors, 61)
  expect_length(unique(colors), 61)
})

test_that("palette metadata has a unique composite key", {
  key <- paste(palette_info$palette_name, palette_info$type, sep = "::")

  expect_identical(anyDuplicated(key), 0L)
  expect_identical(
    length(coloristr:::discrete_palettes),
    35L
  )
})
