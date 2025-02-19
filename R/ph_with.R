#' @title Add objects to current slide using key value syntax
#' @description A sibling of [officer::ph_with] which allows adding several objects to a
#' slide at once. Locations are specfied using the short form syntax. The location and object
#' are passed as key value pairs in the function call (`ph_with_2("short-form" = object)`).
#' Under the hood, [officer::ph_with] is called for each entry. Note that `ph_with_2` does not
#' cover all options from the `ph_location_*` family and also less customization. It is meant as
#' a covenience wrapper for the most common use cases.
#' @param x A `rpptx` object.
#' @param ... Key-value pairs of the form `"short form location" = object`. If the short form is an
#' integer or a string with blanks, you must wrap it in quotes or backticks. The implemented short
#' forms are listed in section `"Short forms"`.
#'
#' @section Short forms:
#' The following short forms are implemented and can be used as the parameter in the function call.
#' The corresponding function from the `ph_location_*` family (called under the hood) is displayed
#' on the right.
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
#' @examples
#' library(officer)
#' x <- read_pptx() |>
#'   add_slide("Two Content") |>
#'   ph_with_2(
#'     `Title 1` = "A title", dt = "Jan. 26, 2025",
#'     `body[2]` = "Body 2", left = "Left side", `6` = "Footer"
#'   )
ph_with_2 <- function(x, ...) {
  dots <- list(...)
  if (length(dots) == 0) {
    return(x)
  }
  loc_strings <- as.list(names(dots))
  ii <- grepl("^\\d+$", loc_strings)
  loc_strings[ii] <- as.integer(loc_strings[ii])
  locations <- lapply(loc_strings, officer::resolve_location)
  for (i in seq_along(dots)) {
    x <- ph_with(x, dots[[i]], locations[[i]])
  }
  x
}
