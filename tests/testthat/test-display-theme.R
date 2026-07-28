with_test_pdf <- function(code) {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(code)
}

test_that("display_palette resolves type before selecting a palette", {
  expect_error(
    display_palette("tidyplots.plasma"),
    "ambiguous",
    fixed = TRUE
  )

  colors <- with_test_pdf(
    display_palette(
      "tidyplots.plasma",
      n = 5,
      type = "continuous"
    )
  )
  expect_length(colors, 5)
  short_name_colors <- with_test_pdf(
    display_palette("plasma", n = 5, type = "continuous")
  )
  expect_identical(short_name_colors, colors)

  expect_error(
    display_palette("tidyplots.friendly", type = "continuous"),
    "not found",
    fixed = TRUE
  )
})

test_that("display functions validate filters and counts", {
  expect_error(
    display_palette("tidyplots.inferno", n = 0),
    "finite positive integer scalar",
    fixed = TRUE
  )
  expect_error(
    display_all_palettes(n = 1.5),
    "finite positive integer scalar",
    fixed = TRUE
  )
  expect_error(
    list_palettes(type = "invalid"),
    "`type` must be one of",
    fixed = TRUE
  )

  displayed <- with_test_pdf(
    display_all_palettes(type = "discrete", source = "tidyplots")
  )
  expect_s3_class(displayed, "data.frame")
  expect_true(all(displayed$type == "discrete"))
  expect_true(all(displayed$source == "tidyplots"))
})

test_that("bold_fonts forwards the face argument", {
  font_theme <- bold_fonts(face = "italic", legend_text_face = "bold")

  expect_identical(font_theme$text$face, "italic")
  expect_identical(font_theme$legend.text$face, "bold")
  expect_s3_class(font_theme, "theme")
})
