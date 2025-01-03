
#' Apply bold fonts to theme elements for easier reading.
#'
#' @param face fontface for all text
#' @param title_size_ratio relative size to original of all titles (plots, axis, legends, strips)
#' @param legend_face fontface for legend text
#'
#' @export
#'
bold_fonts <- function(face = 'bold', title_size_ratio = 1.15, legend_text_face = 'plain'){
  theme(
    text = element_text(face = 'bold'),
    title = element_text(size = rel(title_size_ratio)),
    legend.text = element_text(face = legend_text_face)
  )
}