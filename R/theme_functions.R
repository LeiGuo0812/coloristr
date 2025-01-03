
#' Apply bold fonts to theme elements for easier reading.
#'
#' @param face fontface for all text
#' @param title_size_ratio relative size to original of all titles (plots, axis, legends, strips)
#' @param legend_text_face fontface for legend text
#' @importFrom ggplot2 theme element_text rel
#' @export
#' @examples
#' library(ggplot2)
#' mtcars |> 
#'   ggplot(aes(x = disp, y = mpg)) +
#'   geom_point() +
#'   bold_fonts()

bold_fonts <- function(face = 'bold', title_size_ratio = 1.15, legend_text_face = 'plain'){
  theme(
    text = element_text(face = 'bold'),
    title = element_text(size = rel(title_size_ratio)),
    legend.text = element_text(face = legend_text_face)
  )
}