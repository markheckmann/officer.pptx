# Select shapes on slides -------------------------------------------------


#' Select top-level shapes on slides
#'
#' \if{html}{\out{<a href="https://lifecycle.r-lib.org/articles/stages.html#experimental"><img src="https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg" alt="Experimental lifecycle"></a>}}
#' `shape_select()` returns metadata and XML node references for existing
#' top-level objects on one or more slides. It is intended for workflows where
#' manually placed shapes serve as markers for later processing.
#'
#' This function only selects shapes. It does not insert, replace, hide, or
#' remove slide content.
#'
#' @param x `[rpptx]`\cr An `rpptx` object returned by [officer::read_pptx()].
#' @param slide_idx `[integer]`\cr Slide indexes to search. If `NULL` (default),
#'   all slides are searched.
#' @param text `[character]`\cr Visible text to match.
#' @param name `[character]`\cr Shape names as shown in PowerPoint's Selection Pane.
#' @param id `[character]`\cr Current XML shape IDs. These are slide-local and not
#'   stable across save operations.
#' @param kind `[character]`\cr Shape kinds to include, e.g. `"shape"`,
#'   `"picture"`, `"table"`, `"chart"`, `"graphic_frame"`, `"group"`,
#'   or `"connector"`.
#' @param placeholder `[logical]`\cr If supplied, filter by whether the object is
#'   an actual PowerPoint placeholder.
#' @param ph_type `[character]`\cr Placeholder types to include.
#' @param hidden `[logical]`\cr If supplied, filter by the shape's hidden flag.
#' @param match `[character(1)]`\cr Matching mode for `text` and `name`.
#'   `"contains"` (default) uses fixed substring matching. `"exact"` compares
#'   complete values after trimming leading and trailing whitespace. `"regex"`
#'   uses regular expressions.
#' @return A tibble-like `pptx_shape_selection` object with one row per selected
#'   top-level shape. The `node` list-column contains the underlying XML node.
#'   The selection also carries its source presentation, so it can be piped into
#'   [shape_update()].
#' @export
#' @example inst/ext/examples/example-shape-select.R
shape_select <- function(x, slide_idx = NULL, text = NULL, name = NULL,
                         id = NULL, kind = NULL, placeholder = NULL,
                         ph_type = NULL, hidden = NULL,
                         match = c("contains", "exact", "regex")) {
  assert_class(x, "rpptx")
  match <- match.arg(match)
  slide_idx <- shape_validate_slide_idx(x, slide_idx)

  out <- pptx_shape_inventory(x, slide_idx = slide_idx)
  out <- shape_filter_chr(out, "text", text, match = match)
  out <- shape_filter_chr(out, "name", name, match = match)
  out <- shape_filter_exact(out, "id", id)
  out <- shape_filter_exact(out, "kind", kind)
  out <- shape_filter_lgl(out, "placeholder", placeholder)
  out <- shape_filter_exact(out, "ph_type", ph_type)
  out <- shape_filter_lgl(out, "hidden", hidden)

  new_pptx_shape_selection(out, source = shape_selection_source(x, out), pptx = x)
}


#' @export
print.pptx_shape_selection <- function(x, ...) {
  out <- x
  out$node <- NULL
  class(out) <- setdiff(class(out), "pptx_shape_selection")
  print(out, ...)
  invisible(x)
}


pptx_shape_inventory <- function(x, slide_idx = NULL) {
  slide_idx <- shape_validate_slide_idx(x, slide_idx)

  rows <- vector("list", 0L)
  for (idx in slide_idx) {
    slide <- pptx_slide_get_visible(x, idx)
    nodes <- xml_slide_top_level_shapes(slide)
    if (length(nodes) == 0L) {
      next
    }

    slide_rows <- lapply(seq_along(nodes), function(i) {
      xml_shape_row(nodes[[i]], slide_idx = idx, shape_idx = i)
    })
    rows <- c(rows, slide_rows)
  }

  if (length(rows) == 0L) {
    return(shape_selection_empty())
  }

  out <- dplyr::bind_rows(rows)
  shape_resolve_inherited_geometry(x, out)
}


shape_resolve_inherited_geometry <- function(x, inventory) {
  needs_resolve <- inventory$placeholder &
    is.na(inventory$left) &
    is.na(inventory$top) &
    is.na(inventory$width) &
    is.na(inventory$height)

  if (!any(needs_resolve)) {
    return(inventory)
  }

  for (i in which(needs_resolve)) {
    slide_idx <- inventory$slide_idx[[i]]
    ph_node <- xml_shape_ph(inventory$node[[i]])
    ph_type <- xml_shape_ph_type(ph_node)
    ph_idx <- xml2::xml_attr(ph_node, "idx")

    layout_info <- tryCatch(
      officer:::get_slide_layout(x, slide_idx),
      error = function(e) NULL
    )
    if (is.null(layout_info)) next

    props <- layout_properties(
      x,
      layout = layout_info$layout_name,
      master = layout_info$master_name
    )
    props$ph_idx <- shape_ph_idx_from_layout(props$ph)

    match_idx <- shape_match_layout_ph(props, ph_type, ph_idx)
    if (length(match_idx) == 0L) next
    match_idx <- match_idx[1L]

    inventory$left[[i]] <- props$offx[[match_idx]]
    inventory$top[[i]] <- props$offy[[match_idx]]
    inventory$width[[i]] <- props$cx[[match_idx]]
    inventory$height[[i]] <- props$cy[[match_idx]]
    inventory$rotation[[i]] <- props$rotation[[match_idx]]
  }

  inventory
}


# Extract the idx attribute value from a layout ph definition string.
shape_ph_idx_from_layout <- function(ph) {
  stringr::str_match(ph, 'idx="([0-9]+)"')[, 2]
}


# Match a slide placeholder against layout properties by type and idx.
shape_match_layout_ph <- function(props, ph_type, ph_idx) {
  type_match <- props$type == ph_type
  if (!any(type_match)) {
    return(integer())
  }
  if (is.na(ph_idx)) {
    idx_match <- type_match & is.na(props$ph_idx)
  } else {
    idx_match <- type_match & !is.na(props$ph_idx) & props$ph_idx == ph_idx
  }
  if (any(idx_match)) {
    return(which(idx_match))
  }
  which(type_match)
}


new_pptx_shape_selection <- function(x, source = NULL, pptx = NULL) {
  attr(x, "pptx_shape_selection_source") <- source
  attr(x, "pptx_shape_selection_pptx") <- pptx
  class(x) <- unique(c("pptx_shape_selection", class(x)))
  x
}


shape_selection_source <- function(x, selection) {
  slide_files <- basename(x$presentation$slide_data()$target)
  list(
    package_dir = pptx_package_token(x),
    slide_files = slide_files[selection$slide_idx],
    slide_files_all = slide_files
  )
}


pptx_package_token <- function(x) {
  normalizePath(x$package_dir %||% "", mustWork = FALSE)
}


shape_selection_empty <- function() {
  dplyr::tibble(
    slide_idx = integer(),
    shape_idx = integer(),
    id = character(),
    name = character(),
    text = character(),
    kind = character(),
    placeholder = logical(),
    ph_type = character(),
    hidden = logical(),
    left = numeric(),
    top = numeric(),
    width = numeric(),
    height = numeric(),
    rotation = numeric(),
    description = character(),
    node = list()
  )
}


shape_validate_slide_idx <- function(x, slide_idx = NULL) {
  n_slides <- length(x)
  if (is.null(slide_idx)) {
    return(seq_len(n_slides))
  }
  if (!is.numeric(slide_idx) || anyNA(slide_idx) || any(slide_idx != as.integer(slide_idx))) {
    cli::cli_abort("{.arg slide_idx} must contain valid slide indexes.", call = NULL)
  }
  invalid <- slide_idx[slide_idx < 1L | slide_idx > n_slides]
  if (length(invalid) > 0L) {
    cli::cli_abort(
      "{.arg slide_idx} contains invalid indices: {.val {invalid}} (presentation has {n_slides} slide{?s}).",
      call = NULL
    )
  }
  as.integer(slide_idx)
}


pptx_slide_get_visible <- function(x, slide_idx) {
  slide_file <- basename(x$presentation$slide_data()$target[slide_idx])
  x$slide$get_slide(x$slide$slide_index(slide_file))$get()
}


xml_slide_top_level_shapes <- function(slide) {
  sp_tree <- xml_slide_shape_tree(slide)
  children <- xml2::xml_children(sp_tree)
  children[xml2::xml_name(children) %in% shape_xml_names()]
}


shape_xml_names <- function() {
  c("sp", "pic", "graphicFrame", "grpSp", "cxnSp")
}


xml_shape_row <- function(node, slide_idx, shape_idx) {
  c_nv_pr <- xml_shape_c_nv_pr(node)
  ph <- xml_shape_ph(node)
  frame <- xml_shape_frame(node)

  dplyr::tibble(
    slide_idx = slide_idx,
    shape_idx = shape_idx,
    id = xml2::xml_attr(c_nv_pr, "id"),
    name = xml2::xml_attr(c_nv_pr, "name"),
    text = xml_shape_text(node),
    kind = xml_shape_kind(node),
    placeholder = !is_xml_missing(ph),
    ph_type = xml_shape_ph_type(ph),
    hidden = identical(xml2::xml_attr(c_nv_pr, "hidden"), "1"),
    left = frame$left,
    top = frame$top,
    width = frame$width,
    height = frame$height,
    rotation = frame$rotation,
    description = xml2::xml_attr(c_nv_pr, "descr"),
    node = list(node)
  )
}


xml_shape_c_nv_pr <- function(node) {
  path <- switch(xml2::xml_name(node),
    sp = "p:nvSpPr/p:cNvPr",
    pic = "p:nvPicPr/p:cNvPr",
    graphicFrame = "p:nvGraphicFramePr/p:cNvPr",
    grpSp = "p:nvGrpSpPr/p:cNvPr",
    cxnSp = "p:nvCxnSpPr/p:cNvPr",
    NA_character_
  )
  xml2::xml_child(node, path)
}


xml_shape_ph <- function(node) {
  path <- switch(xml2::xml_name(node),
    sp = "p:nvSpPr/p:nvPr/p:ph",
    pic = "p:nvPicPr/p:nvPr/p:ph",
    graphicFrame = "p:nvGraphicFramePr/p:nvPr/p:ph",
    grpSp = "p:nvGrpSpPr/p:nvPr/p:ph",
    cxnSp = "p:nvCxnSpPr/p:nvPr/p:ph",
    NA_character_
  )
  xml2::xml_child(node, path)
}


xml_shape_ph_type <- function(ph) {
  if (is_xml_missing(ph)) {
    return(NA_character_)
  }
  type <- xml2::xml_attr(ph, "type")
  type %||% "body"
}


xml_shape_frame <- function(node) {
  xfrm <- xml_shape_xfrm(node)
  if (is_xml_missing(xfrm)) {
    return(list(
      left = NA_real_, top = NA_real_, width = NA_real_,
      height = NA_real_, rotation = NA_real_
    ))
  }

  off <- xml2::xml_child(xfrm, "a:off")
  ext <- xml2::xml_child(xfrm, "a:ext")
  rot <- xml2::xml_attr(xfrm, "rot")
  rot <- if (is.na(rot)) NA_real_ else -rotation_to_degree(as.numeric(rot))

  list(
    left = emu_to_inch(as.numeric(xml2::xml_attr(off, "x"))),
    top = emu_to_inch(as.numeric(xml2::xml_attr(off, "y"))),
    width = emu_to_inch(as.numeric(xml2::xml_attr(ext, "cx"))),
    height = emu_to_inch(as.numeric(xml2::xml_attr(ext, "cy"))),
    rotation = rot
  )
}


is_xml_missing <- function(x) {
  inherits(x, "xml_missing")
}


xml_shape_xfrm <- function(node) {
  path <- switch(xml2::xml_name(node),
    sp = "p:spPr/a:xfrm",
    pic = "p:spPr/a:xfrm",
    graphicFrame = "p:xfrm",
    grpSp = "p:grpSpPr/a:xfrm",
    cxnSp = "p:spPr/a:xfrm",
    NA_character_
  )
  xml2::xml_child(node, path)
}


xml_shape_text <- function(node) {
  if (identical(xml2::xml_name(node), "grpSp")) {
    return(NA_character_)
  }
  xml2::xml_text(node)
}


xml_shape_kind <- function(node) {
  switch(xml2::xml_name(node),
    sp = "shape",
    pic = "picture",
    graphicFrame = xml_graphic_frame_kind(node),
    grpSp = "group",
    cxnSp = "connector",
    "unknown"
  )
}


xml_graphic_frame_kind <- function(node) {
  graphic_data <- xml2::xml_find_first(node, ".//a:graphic/a:graphicData")
  uri <- xml2::xml_attr(graphic_data, "uri")
  if (is.na(uri)) {
    return("graphic_frame")
  }
  if (grepl("table", uri, fixed = TRUE)) {
    return("table")
  }
  if (grepl("chart", uri, fixed = TRUE)) {
    return("chart")
  }
  if (grepl("diagram", uri, fixed = TRUE)) {
    return("diagram")
  }
  "graphic_frame"
}


shape_filter_chr <- function(x, var, value = NULL, match = "contains") {
  if (is.null(value)) {
    return(x)
  }
  keep <- shape_chr_match(x[[var]], value, match = match)
  x[keep, , drop = FALSE]
}


shape_filter_exact <- function(x, var, value = NULL) {
  if (is.null(value)) {
    return(x)
  }
  keep <- !is.na(x[[var]]) & x[[var]] %in% value
  x[keep, , drop = FALSE]
}


shape_filter_lgl <- function(x, var, value = NULL) {
  if (is.null(value)) {
    return(x)
  }
  if (!is.logical(value) || anyNA(value)) {
    cli::cli_abort("{.arg {var}} must be {.cls logical}.", call = NULL)
  }
  keep <- x[[var]] %in% value
  x[keep, , drop = FALSE]
}


shape_chr_match <- function(x, pattern, match = "contains") {
  if (!is.character(pattern) || anyNA(pattern)) {
    cli::cli_abort("Text matching patterns must be character values without missing values.", call = NULL)
  }
  if (length(pattern) == 0L) {
    return(rep(TRUE, length(x)))
  }

  switch(match,
    contains = vapply(x, function(value) {
      !is.na(value) && any(vapply(pattern, grepl, logical(1), x = value, fixed = TRUE))
    }, logical(1)),
    exact = trimws(x) %in% trimws(pattern),
    regex = vapply(x, function(value) {
      !is.na(value) && any(vapply(pattern, grepl, logical(1), x = value, perl = TRUE))
    }, logical(1))
  )
}
