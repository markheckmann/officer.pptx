#' @export
#' @title Add a slide (patched version with dots)
#' @description Add a slide into a pptx presentation.
#' @param x an rpptx object
#' @param layout slide layout name to use
#' @param master master layout name where \code{layout} is located
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Key-value pairs of the form `"short form location" = object`
#' passed to [phs_with].
#' @param .dots List of type `"short form location" = object`. Alternative to using the dots `...`.
#' @example inst/ext/examples/example-add-slide.R
add_slide <- function(x, layout = "Title and Content", master = NULL, ..., .dots = NULL) {
  x <- officer::add_slide(x, layout = layout, master = master)
  dots <- rlang::list2(...)
  dots <- utils::modifyList(dots, .dots %||% list())
  if (length(dots) > 0) {
    x <- phs_with(x, !!!dots)
  }
  x
}
