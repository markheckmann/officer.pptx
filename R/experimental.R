# experimental function. Might be removed again.

# temp
# count occurences of a text pattern on a slide (inside a shape)
slide_pattern_count_temp <- function(x, slide_idx, pattern, fixed = TRUE) {
  assert_class(x, "rpptx")
  stop_if_not_in_slide_range(x, slide_idx)
  if (fixed && !inherits(pattern, "stringr_fixed")) {
    pattern <- stringr::fixed(pattern)
  }
  x$cursor <- slide_idx # necessary as used by
  shapes <- x |> pptx_shapes_on_slide(slide_idx = slide_idx, pattern = pattern)
  length(shapes)
}


# temp
slides_text_count_temp <- function(x, pattern, slide_idx = NULL, fixed = TRUE) {
  assert_class(x, "rpptx")
  if (fixed && !inherits(pattern, "stringr_fixed")) {
    pattern <- stringr::fixed(pattern)
  }
  n_slides <- length(x)
  if (n_slides == 0) {
    cli::cli_alert_danger("Presentation has no slides")
    return(integer())
  }
  ii <- slide_idx %||% seq_len(n_slides)
  vapply(ii, slide_pattern_count_temp, x = x, pattern = pattern, integer(1))
}


# TODO: tests
# x <- read_pptx() |>
#   add_slide("Title and Content", title = "xxx", body = "xxx") |>
#   add_slide("Title and Content", title = "xxx", body = "xx") |>
#   add_slide("Title and Content", title = "xxx", body = "x")
# slide_pattern_count_temp(x, 1, "xx")
# slides_text_count_temp(x, "xx")
