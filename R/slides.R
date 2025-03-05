# DRAW ----------------------


#' Plot a slide
#'
#' Generate an wireframe-like image of the structrual elements on a slide
#' (shapes, placeholders, images etc.).  Helps to get a quick impression what
#' is on that slide.
#'
#' @param x `[rpptx]`\cr An [officer] object. See [officer::read_pptx()].
#' @param slide_idx `[numeric]`\cr Index of slides to process. If `NULL` (default),
#'   all slides are processed.
#' @param highlight TBD
#' @export
#' @keywords internal
slide_show <- function(x, slide_idx, highlight = NULL) {
  w <- slide_size(x)
  layout_summary(x) # layou summary shown  in print
  pptx_shapes_info(x)
}
