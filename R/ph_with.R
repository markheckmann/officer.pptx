#' @title Fill multiple placeholders using key value syntax
#' @description A sibling of [officer::ph_with] that fills mutiple placeholders at once. Placeholder locations are
#'   specfied using the short form syntax. The location and corresponding object are passed as key value pairs
#'   (`phs_with("short form location" = object)`). Under the hood, [officer::ph_with] is called for each pair. Note
#'   that `phs_with` does not cover all options from the `ph_location_*` family and is also less customization. It is a
#'   covenience wrapper for the most common use cases. The implemented short forms are listed in section
#'   `"Short forms"`.
#' @param x A `rpptx` object.
#' @param ... Key-value pairs of the form `"short form location" = object`. If the short form is an integer or a string
#'   with blanks, you must wrap it in quotes or backticks.
#' @param .dots List of key-value pairs `"short form location" = object`. Alternative to `...`.
#' @param .slide_idx Numeric indexes of slides to process. `NULL` (default) processes the current slide only. Use
#'   keyowrd `all` for all slides.
#' @section Short forms: The following short forms are implemented and can be used as the parameter in the function
#'   call. The corresponding function from the `ph_location_*` family (called under the hood) is displayed on the
#'   right.
#'
#' | **Short form** | **Description**                                   | **Location function**           |
#' |----------------|---------------------------------------------------|---------------------------------|
#' | `"left"`       | Keyword string                                    | `ph_location_left()`            |
#' | `"right"`      | Keyword string                                    | `ph_location_right()`           |
#' | `"fullsize"`   | Keyword string                                    | `ph_location_fullsize()`        |
#' | `"body [1]"`   | String: type + index in brackets (`1` if omitted) | `ph_location_type("body", 1)`   |
#' | `"my_label"`   | Any string not matching a keyword or type         | `ph_location_label("my_label")` |
#' | `1`            | Length 1 integer                                  | `ph_location_id(1)`             |
#'
#' @export
#' @example inst/ext/examples/example-phs-with.R
phs_with <- function(x, ..., .dots = NULL, .slide_idx = NULL) {
  dots <- modifyList(list(...), .dots %||% list())
  if (length(dots) == 0) {
    return(x)
  }
  .slide_idx <- .slide_idx %||% x$cursor # default is current slide
  if (is.character(.slide_idx) && .slide_idx == "all") {
    .slide_idx <- seq_len(length(x))
  }
  officer:::stop_if_not_in_slide_range(x, .slide_idx)

  loc_strings <- as.list(names(dots))
  ii <- grepl("^\\d+$", loc_strings) # find integer short-forms
  loc_strings[ii] <- as.integer(loc_strings[ii])
  locations <- lapply(loc_strings, officer::resolve_location)

  .old_cursor <- x$cursor
  for (slide_idx in .slide_idx) {
    x$cursor <- slide_idx # pw_with always uses the current slide
    for (i in seq_along(dots)) {
      x <- ph_with(x, dots[[i]], locations[[i]])
    }
  }
  x$cursor <- .old_cursor
  x
}
