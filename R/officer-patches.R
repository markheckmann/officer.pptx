#' @export
#' @title Add a slide (patched version with dots)
#' @description Add a slide into a pptx presentation.
#' @param x an rpptx object
#' @param layout slide layout name to use
#' @param master master layout name where \code{layout} is located
#' @param ... Key-value pairs of the form `"short form location" = object` passed to [phs_with].
#' @param .dots List with key-value pairs `"short form location" = object`. Alternative to `...`.
#' @example inst/ext/examples/example-add-slide.R
add_slide <- function(x, layout = "Title and Content", master = NULL, ..., .dots = NULL) {
  x <- officer::add_slide(x, layout = layout, master = master)
  dots <- utils::modifyList(list(...), .dots %||% list())
  if (length(dots) > 0) {
    x <- phs_with(x, .dots = dots)
  }
  x
}
