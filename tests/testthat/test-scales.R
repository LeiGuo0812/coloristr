expect_midpoint_mapping <- function(scale, values) {
  scale$train(values)
  expect_identical(
    unname(scale$map(values)),
    unname(scale$palette(c(0, 0.5, 1)))
  )
}

test_that("public scale signatures remain unchanged", {
  expect_identical(
    names(formals(scale_color_discrete_cr)),
    c("palette_name", "reverse", "...")
  )
  expect_identical(
    names(formals(scale_fill_discrete_cr)),
    c("palette_name", "reverse", "...")
  )
  expect_identical(
    names(formals(scale_color_continuous_cr)),
    c("palette_name", "reverse", "...")
  )
  expect_identical(
    names(formals(scale_fill_continuous_cr)),
    c("palette_name", "reverse", "...")
  )
  expect_identical(
    names(formals(scale_color_diverging_cr)),
    c("palette_name", "midpoint", "reverse", "...")
  )
  expect_identical(
    names(formals(scale_fill_diverging_cr)),
    c("palette_name", "midpoint", "reverse", "...")
  )
})

test_that("all scale constructors return ggplot2 scales", {
  scales <- list(
    scale_color_discrete_cr(),
    scale_fill_discrete_cr(),
    scale_color_continuous_cr(),
    scale_fill_continuous_cr(),
    scale_color_diverging_cr(),
    scale_fill_diverging_cr()
  )

  for (scale in scales) {
    expect_s3_class(scale, "Scale")
  }
})

test_that("invalid discrete palettes fail during scale construction", {
  expect_error(
    scale_color_discrete_cr("does.not.exist"),
    "not found",
    fixed = TRUE
  )
  expect_error(
    scale_fill_discrete_cr("does.not.exist"),
    "not found",
    fixed = TRUE
  )
})

test_that("discrete scales build without deprecated scale_name warnings", {
  data <- data.frame(
    x = 1:3,
    y = 1:3,
    group = factor(1:3)
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x, y, color = group)
  ) +
    ggplot2::geom_point() +
    scale_color_discrete_cr()

  expect_no_warning(ggplot2::ggplot_build(plot))
})

test_that("diverging scales map the data midpoint to the palette midpoint", {
  color_scale <- scale_color_diverging_cr(midpoint = 0)
  fill_scale <- scale_fill_diverging_cr(midpoint = 50)

  expect_midpoint_mapping(color_scale, c(-1, 0, 1))
  expect_midpoint_mapping(fill_scale, c(0, 50, 100))
})

test_that("diverging midpoint respects transformations", {
  scale <- scale_color_diverging_cr(
    midpoint = 10,
    transform = "log10"
  )
  transformed_values <- scale$transform(c(1, 10, 100))

  expect_midpoint_mapping(scale, transformed_values)
})

test_that("gradient colour overrides are normalized", {
  expected <- c("#000000", "#FFFFFF")

  british <- scale_color_continuous_cr(
    colours = c("black", "white")
  )
  american <- scale_color_continuous_cr(
    colors = c("black", "white")
  )

  expect_identical(british$palette(c(0, 1)), expected)
  expect_identical(american$palette(c(0, 1)), expected)
  expect_error(
    scale_color_continuous_cr(
      colours = c("black", "white"),
      colors = c("red", "blue")
    ),
    "at most one",
    fixed = TRUE
  )
})
