# Add shapes ---------------------------------------------------------------


#' Add regular slide shapes
#'
#' `shape_add()` adds ordinary non-placeholder PowerPoint shapes to one or more
#' slides. It creates top-level regular shapes (`p:sp`) and returns the modified
#' `rpptx` object.
#'
#' New shapes are not layout placeholders. They are created with an empty
#' `<p:nvPr/>` node and no `<p:ph/>` marker.
#'
#' @param x `[rpptx]`\cr An `rpptx` object returned by [officer::read_pptx()].
#' @param left,top,width,height `[numeric]`\cr Shape position and size in inches.
#' @param text `[character]`\cr Text to put in the shape.
#' @param slide_idx `[integer]`\cr Slide indexes in visible presentation order.
#'   If `NULL`, shapes are added to the current slide.
#' @param geometry `[character]`\cr Preset PowerPoint geometry, e.g. `"rect"`,
#'   `"roundRect"`, `"ellipse"`, or `"rightArrow"`.
#' @param background `[character]`\cr Solid background color. Use
#'   `"transparent"` for no fill.
#' @param line `[sp_line]`\cr An [officer::sp_line()] object, or a list of
#'   `sp_line` objects, used as outline style.
#' @param rotation `[numeric]`\cr Shape rotation in degrees. Positive values use
#'   the same convention as [officer::ph_location()].
#' @param name `[character]`\cr Shape name as shown in PowerPoint's Selection Pane.
#'   If `NULL`, names are generated automatically.
#' @param description `[character]`\cr Alternative text description. Use `NA` to
#'   omit the description.
#' @param hidden `[logical]`\cr Whether to hide the shape.
#' @return The modified `rpptx` object.
#' @export
#' @example inst/ext/examples/example-shape-add.R
shape_add <- function(x, left, top, width, height, text = "", slide_idx = NULL,
                      geometry = "rect", background = "transparent",
                      line = officer::sp_line(), rotation = 0, name = NULL,
                      description = NA_character_, hidden = FALSE) {
  assert_class(x, "rpptx")

  n <- shape_add_n(
    left = left, top = top, width = width, height = height, text = text,
    slide_idx = slide_idx, geometry = geometry, background = background,
    line = line, rotation = rotation, name = name, description = description,
    hidden = hidden
  )

  args <- shape_add_args(
    x = x, n = n,
    left = left,
    top = top,
    width = width,
    height = height,
    text = text,
    slide_idx = slide_idx,
    geometry = geometry,
    background = background,
    line = line,
    rotation = rotation,
    name = name,
    description = description,
    hidden = hidden
  )

  nodes <- vector("list", n)
  for (i in seq_len(n)) {
    nodes[[i]] <- xml_shape_new_regular(args$geometry[[i]])
    shape_update_node(nodes[[i]], args, i)
    if (xml_shape_is_placeholder(nodes[[i]])) {
      cli::cli_abort("Internal error: {.fn shape_add} created a placeholder shape.", call = NULL)
    }
  }

  old_cursor <- x$cursor
  on.exit(x$cursor <- old_cursor, add = TRUE)

  for (i in seq_len(n)) {
    slide <- pptx_shape_add_slide_get(x, args$slide_idx[[i]])
    sp_tree <- xml_slide_shape_tree(slide)
    xml_slide_add_top_level_shape(sp_tree, nodes[[i]])
  }

  x
}


shape_add_args <- function(x, n, left, top, width, height, text, slide_idx,
                           geometry, background, line, rotation, name,
                           description, hidden) {
  list(
    left = shape_recycle_num(left, n, "left", finite = TRUE),
    top = shape_recycle_num(top, n, "top", finite = TRUE),
    width = shape_recycle_num(width, n, "width", positive = TRUE, finite = TRUE),
    height = shape_recycle_num(height, n, "height", positive = TRUE, finite = TRUE),
    text = shape_recycle_chr(text, n, "text", allow_na = FALSE),
    slide_idx = shape_recycle_slide_idx(x, slide_idx, n),
    geometry = shape_recycle_geometry(geometry, n),
    background = shape_recycle_background(background, n),
    line = shape_recycle_line(line, n),
    rotation = shape_recycle_num(rotation, n, "rotation", finite = TRUE),
    name = shape_recycle_shape_name(name, n),
    description = shape_recycle_chr(description, n, "description", allow_na = TRUE),
    hidden = shape_recycle_lgl(hidden, n, "hidden")
  )
}


shape_add_n <- function(left, top, width, height, text, slide_idx, geometry,
                        background, line, rotation, name, description, hidden) {
  sizes <- c(
    shape_value_size(left, "left"),
    shape_value_size(top, "top"),
    shape_value_size(width, "width"),
    shape_value_size(height, "height"),
    shape_value_size(text, "text"),
    shape_value_size(slide_idx, "slide_idx"),
    shape_value_size(geometry, "geometry"),
    shape_value_size(background, "background"),
    shape_value_size(line, "line"),
    shape_value_size(rotation, "rotation"),
    shape_value_size(name, "name"),
    shape_value_size(description, "description"),
    shape_value_size(hidden, "hidden")
  )

  n <- max(sizes)
  invalid <- unique(sizes[sizes != 1L & sizes != n])
  if (length(invalid) > 0L) {
    cli::cli_abort(
      "Arguments must have length 1 or the common number of shapes ({n}).",
      call = NULL
    )
  }
  n
}


shape_value_size <- function(x, arg) {
  if (is.null(x)) {
    return(1L)
  }
  if (inherits(x, "sp_line")) {
    return(1L)
  }
  size <- length(x)
  if (size == 0L) {
    cli::cli_abort("{.arg {arg}} must not be empty.", call = NULL)
  }
  size
}


shape_recycle_slide_idx <- function(x, slide_idx, n) {
  if (is.null(slide_idx)) {
    if (length(x) == 0L) {
      cli::cli_abort("{.arg x} has no slides. Add a slide before calling {.fn shape_add}.", call = NULL)
    }
    return(rep(list(NA_integer_), n))
  }
  slide_idx <- shape_validate_slide_idx(x, slide_idx)
  shape_recycle_atomic(slide_idx, n, "slide_idx")
}


shape_recycle_geometry <- function(x, n) {
  values <- shape_recycle_chr(x, n, "geometry", allow_na = FALSE)
  invisible(lapply(values, function(value) {
    officer::shape_properties_tags(ph = "", geom = value, bg = NULL, ln = NULL)
  }))
  values
}


shape_recycle_shape_name <- function(x, n) {
  if (is.null(x)) {
    return(as.list(paste0("Shape ", seq_len(n))))
  }
  shape_recycle_chr(x, n, "name", allow_na = FALSE)
}


xml_shape_new_regular <- function(geometry = "rect") {
  props <- officer::shape_properties_tags(
    left = 0,
    top = 0,
    width = 1,
    height = 1,
    label = "",
    ph = "",
    bg = NULL,
    ln = NULL,
    geom = geometry
  )
  xml <- paste0(
    '<p:sp xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main" ',
    'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" ',
    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
    props,
    "<p:txBody><a:bodyPr/><a:lstStyle/><a:p><a:r><a:rPr/><a:t/></a:r></a:p></p:txBody>",
    "</p:sp>"
  )
  xml2::read_xml(xml)
}


pptx_shape_add_slide_get <- function(x, slide_idx) {
  if (is.na(slide_idx)) {
    return(x$slide$get_slide(x$cursor)$get())
  }
  pptx_slide_get_visible(x, slide_idx)
}


xml_slide_shape_tree <- function(slide) {
  xml2::xml_child(slide, "p:cSld/p:spTree")
}


xml_slide_add_top_level_shape <- function(sp_tree, node) {
  ext_lst <- xml2::xml_child(sp_tree, "p:extLst")
  if (is_xml_missing(ext_lst)) {
    xml2::xml_add_child(sp_tree, node)
  } else {
    xml2::xml_add_sibling(ext_lst, node, .where = "before")
  }
  invisible(sp_tree)
}


xml_shape_is_placeholder <- function(node) {
  length(xml2::xml_find_all(node, ".//p:nvPr/p:ph")) > 0L
}
