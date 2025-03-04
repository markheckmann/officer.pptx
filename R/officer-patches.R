#' @export
#' @title Add a slide (patched version with dots)
#' @description Add a slide into a pptx presentation.
#' @param x an rpptx object
#' @param layout slide layout name to use
#' @param master master layout name where \code{layout} is located
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Key-value pairs of the form `"short form location" = object`
#' @param .phs_with List of type `"short form location" = object`. Alternative to using the dots `...`.
#' passed to [phs_with].
#' @example inst/ext/examples/example-add-slide.R
add_slide <- function(x, layout = "Title and Content", master = NULL, ..., .phs_with = NULL) {
  x <- officer::add_slide(x, layout = layout, master = master)
  dots <- rlang::list2(...)
  dots <- utils::modifyList(dots, .phs_with %||% list())
  if (length(dots) > 0) {
    x <- phs_with(x, !!!dots)
  }
  x
}
