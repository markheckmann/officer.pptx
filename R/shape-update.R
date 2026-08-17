# Update selected shapes ---------------------------------------------------


#' Update selected slide shapes
#'
#' \if{html}{\out{<a href="https://lifecycle.r-lib.org/articles/stages.html#experimental"><img src="https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg" alt="Experimental lifecycle"></a>}}
#' `shape_update()` modifies existing top-level shapes returned by
#' [shape_select()]. It edits the selected XML nodes in place and returns the
#' modified `rpptx` object. Selections can be supplied explicitly or piped from
#' [shape_select()].
#'
#' Selection is based on existing slide objects, not PowerPoint layout
#' placeholders. Ordinary non-placeholder shapes are supported and are the main
#' use case.
#'
#' @param x `[rpptx]` or `[pptx_shape_selection]`\cr An `rpptx` object returned
#'   by [officer::read_pptx()], or a selection returned by [shape_select()] for
#'   use in a pipeline.
#' @param selection `[pptx_shape_selection]`\cr A selection returned by
#'   [shape_select()] for `x`. Leave `NULL` when `x` is already a piped
#'   selection.
#' @param text `[character]`\cr Replacement text. If supplied, the complete text
#'   of each selected text-capable shape is replaced. The first existing text run
#'   keeps its formatting.
#' @param background `[character]`\cr Solid background color. Use
#'   `"transparent"` to force no fill.
#' @param line `[sp_line]`\cr An [officer::sp_line()] object, or a list of
#'   `sp_line` objects, used as outline style.
#' @param left,top,width,height `[numeric]`\cr Finite shape position and size in inches.
#'   `NULL` leaves the current value unchanged.
#' @param rotation `[numeric]`\cr Finite shape rotation in degrees. Positive
#'   values use the same convention as [officer::ph_location()].
#' @param name `[character]`\cr Shape name as shown in PowerPoint's Selection Pane.
#' @param description `[character]`\cr Alternative text description. Use `NA` to
#'   remove an existing description.
#' @param hidden `[logical]`\cr Whether to hide the shape.
#' @param empty `[character(1)]`\cr What to do with an empty selection. The
#'   default, `"error"`, fails early. Use `"ignore"` to return `x` unchanged.
#' @return The modified `rpptx` object.
#' @export
#' @example inst/ext/examples/example-shape-update.R
shape_update <- function(x, selection = NULL, text = NULL, background = NULL,
                         line = NULL, left = NULL, top = NULL, width = NULL,
                         height = NULL, rotation = NULL, name = NULL,
                         description = NULL, hidden = NULL,
                         empty = c("error", "ignore")) {
  empty <- match.arg(empty)
  inputs <- shape_update_inputs(x, selection)
  x <- inputs$x
  selection <- inputs$selection

  selection <- shape_selection_validate(x, selection, empty = empty)
  n <- nrow(selection)
  if (n == 0L) {
    return(x)
  }

  args <- shape_update_args(
    n = n,
    text = text,
    background = background,
    line = line,
    left = left,
    top = top,
    width = width,
    height = height,
    rotation = rotation,
    name = name,
    description = description,
    hidden = hidden
  )
  shape_update_validate_capabilities(selection, args)

  old_cursor <- x$cursor
  on.exit(x$cursor <- old_cursor, add = TRUE)

  for (i in seq_len(n)) {
    node <- selection$node[[i]]
    shape_update_node(node, args, i)
  }

  x
}


shape_update_inputs <- function(x, selection = NULL) {
  if (inherits(x, "pptx_shape_selection")) {
    if (!is.null(selection)) {
      cli::cli_abort(
        "Do not supply {.arg selection} when piping a {.cls pptx_shape_selection} into {.fn shape_update}.",
        call = NULL
      )
    }
    pptx <- attr(x, "pptx_shape_selection_pptx")
    if (!inherits(pptx, "rpptx")) {
      cli::cli_abort(
        "Piped {.arg x} has no source presentation. Rerun {.fn shape_select}.",
        call = NULL
      )
    }
    return(list(x = pptx, selection = x))
  }

  assert_class(x, "rpptx")
  list(x = x, selection = selection)
}


shape_update_args <- function(n, text = NULL, background = NULL, line = NULL,
                              left = NULL, top = NULL, width = NULL,
                              height = NULL, rotation = NULL, name = NULL,
                              description = NULL, hidden = NULL) {
  list(
    text = shape_recycle_chr(text, n, "text", allow_na = FALSE),
    background = shape_recycle_background(background, n),
    line = shape_recycle_line(line, n),
    left = shape_recycle_num(left, n, "left", finite = TRUE, scale = 914400),
    top = shape_recycle_num(top, n, "top", finite = TRUE, scale = 914400),
    width = shape_recycle_num(width, n, "width", positive = TRUE, finite = TRUE, scale = 914400),
    height = shape_recycle_num(height, n, "height", positive = TRUE, finite = TRUE, scale = 914400),
    rotation = shape_recycle_num(rotation, n, "rotation", finite = TRUE, scale = 60000),
    name = shape_recycle_chr(name, n, "name", allow_na = FALSE),
    description = shape_recycle_chr(description, n, "description", allow_na = TRUE),
    hidden = shape_recycle_lgl(hidden, n, "hidden")
  )
}


shape_update_node <- function(node, args, i) {
  xml_shape_set_text(node, shape_arg_value(args$text, i))
  xml_shape_set_background(node, shape_arg_value(args$background, i))
  xml_shape_set_line(node, shape_arg_value(args$line, i))
  xml_shape_set_frame(
    node,
    left = shape_arg_value(args$left, i),
    top = shape_arg_value(args$top, i),
    width = shape_arg_value(args$width, i),
    height = shape_arg_value(args$height, i),
    rotation = shape_arg_value(args$rotation, i)
  )
  xml_shape_set_metadata(
    node,
    name = shape_arg_value(args$name, i),
    description = shape_arg_value(args$description, i),
    hidden = shape_arg_value(args$hidden, i)
  )
}


shape_arg_value <- function(x, i) {
  if (length(x) == 0L) {
    return(NULL)
  }
  x[[i]]
}


shape_selection_validate <- function(x, selection, empty = "error") {
  if (!inherits(selection, "pptx_shape_selection")) {
    cli::cli_abort(
      "{.arg selection} must be returned by {.fn shape_select}.",
      call = NULL
    )
  }
  if (!"node" %in% names(selection)) {
    cli::cli_abort("{.arg selection} must contain a {.field node} column.", call = NULL)
  }
  if (nrow(selection) == 0L) {
    if (identical(empty, "error")) {
      cli::cli_abort("{.arg selection} is empty.", call = NULL)
    }
    return(selection)
  }

  source <- attr(selection, "pptx_shape_selection_source")
  if (is.null(source)) {
    cli::cli_abort(
      "{.arg selection} has no source metadata. Rerun {.fn shape_select}.",
      call = NULL
    )
  }
  if (!identical(source$package_dir, pptx_package_token(x))) {
    cli::cli_abort(
      "{.arg selection} was created from a different presentation. Rerun {.fn shape_select}.",
      call = NULL
    )
  }

  current_slide_files <- basename(x$presentation$slide_data()$target)
  if (!identical(source$slide_files_all, current_slide_files)) {
    cli::cli_abort(
      "{.arg selection} is stale because the slide order changed. Rerun {.fn shape_select}.",
      call = NULL
    )
  }
  for (i in seq_len(nrow(selection))) {
    slide <- pptx_slide_get_visible(x, selection$slide_idx[[i]])
    nodes <- xml_slide_top_level_shapes(slide)
    is_current <- any(vapply(nodes, identical, logical(1), selection$node[[i]]))
    if (!is_current) {
      cli::cli_abort(
        "{.arg selection} contains stale shape nodes. Rerun {.fn shape_select}.",
        call = NULL
      )
    }
  }

  selection
}


shape_update_validate_capabilities <- function(selection, args) {
  kinds <- vapply(selection$node, xml_shape_kind, character(1))

  if (shape_arg_requested(args$text)) {
    shape_abort_unsupported(kinds, c("shape", "connector"), "text")
    missing_text <- vapply(selection$node, xml_shape_has_text_runs, logical(1))
    if (any(!missing_text)) {
      cli::cli_abort(
        "Selected shape{?s} cannot be updated with {.arg text} because {?it/they} contain{?s/} no text runs.",
        call = NULL
      )
    }
  }
  if (shape_arg_requested(args$background)) {
    shape_abort_unsupported(kinds, "shape", "background")
  }
  if (shape_arg_requested(args$line)) {
    shape_abort_unsupported(kinds, c("shape", "connector", "picture"), "line")
  }
  if (shape_any_frame_arg(args)) {
    missing_xfrm <- vapply(selection$node, function(node) {
      is_xml_missing(xml_shape_xfrm(node))
    }, logical(1))
    if (any(missing_xfrm)) {
      cli::cli_abort(
        "Selected shape{?s} cannot be resized or moved because {?it/they} have{?s/} no transform XML.",
        call = NULL
      )
    }
  }
}


shape_abort_unsupported <- function(kinds, supported, arg) {
  unsupported <- unique(kinds[kinds %nin% supported])
  if (length(unsupported) > 0L) {
    cli::cli_abort(
      "{.arg {arg}} is not supported for selected shape kind{?s}: {.val {unsupported}}.",
      call = NULL
    )
  }
}


shape_any_frame_arg <- function(args) {
  any(vapply(args[c("left", "top", "width", "height", "rotation")], shape_arg_requested, logical(1)))
}


shape_arg_requested <- function(x) {
  length(x) > 0L
}


shape_recycle_chr <- function(x, n, arg, allow_na = FALSE) {
  if (is.null(x)) {
    return(list())
  }
  if (!is.character(x) || (!allow_na && anyNA(x))) {
    cli::cli_abort("{.arg {arg}} must be a character vector without missing values.", call = NULL)
  }
  shape_recycle_atomic(x, n, arg)
}


shape_recycle_background <- function(x, n) {
  if (is.null(x)) {
    return(list())
  }
  if (!is.character(x) || anyNA(x)) {
    cli::cli_abort("{.arg background} must be a character vector without missing values.", call = NULL)
  }
  values <- shape_recycle_atomic(x, n, "background")
  invisible(lapply(values, function(value) {
    if (!identical(value, "transparent")) {
      officer::solid_fill(value)
    }
  }))
  values
}


shape_recycle_line <- function(x, n) {
  if (is.null(x)) {
    return(list())
  }
  if (inherits(x, "sp_line")) {
    return(rep(list(x), n))
  }
  if (!is.list(x) || !all(vapply(x, inherits, logical(1), "sp_line"))) {
    cli::cli_abort("{.arg line} must be an {.fn officer::sp_line} object or a list of them.", call = NULL)
  }
  if (length(x) == 1L) {
    return(rep(x, n))
  }
  if (length(x) != n) {
    cli::cli_abort("{.arg line} must have length 1 or the number of selected shapes ({n}).", call = NULL)
  }
  x
}


shape_recycle_num <- function(x, n, arg, positive = FALSE, finite = FALSE,
                              scale = NULL) {
  if (is.null(x)) {
    return(list())
  }
  if (!is.numeric(x) || anyNA(x)) {
    cli::cli_abort("{.arg {arg}} must be a numeric vector without missing values.", call = NULL)
  }
  if (finite && any(!is.finite(x))) {
    cli::cli_abort("{.arg {arg}} must contain finite values.", call = NULL)
  }
  if (!is.null(scale) && any(abs(x * scale) > .Machine$integer.max)) {
    cli::cli_abort("{.arg {arg}} contains values outside the supported range.", call = NULL)
  }
  if (positive && any(x <= 0)) {
    cli::cli_abort("{.arg {arg}} must contain positive values.", call = NULL)
  }
  shape_recycle_atomic(x, n, arg)
}


shape_recycle_lgl <- function(x, n, arg) {
  if (is.null(x)) {
    return(list())
  }
  if (!is.logical(x) || anyNA(x)) {
    cli::cli_abort("{.arg {arg}} must be a logical vector without missing values.", call = NULL)
  }
  shape_recycle_atomic(x, n, arg)
}


shape_recycle_atomic <- function(x, n, arg) {
  if (length(x) == 1L) {
    x <- rep(x, n)
  }
  if (length(x) != n) {
    cli::cli_abort("{.arg {arg}} must have length 1 or the number of selected shapes ({n}).", call = NULL)
  }
  as.list(x)
}


xml_shape_has_text_runs <- function(node) {
  length(xml_get_runs(node)) > 0L
}


xml_shape_set_text <- function(node, text = NULL) {
  if (is.null(text)) {
    return(invisible(node))
  }
  runs <- xml_get_runs(node)
  text_node <- xml_shape_run_text_node(runs[[1]])
  xml2::xml_text(text_node) <- text
  if (length(runs) > 1L) {
    for (run in runs[-1]) {
      text_node <- xml_shape_run_text_node(run)
      xml2::xml_text(text_node) <- ""
    }
  }
  xml_shape_mark_text_dirty(runs)
  invisible(node)
}


xml_shape_run_text_node <- function(run) {
  text_node <- xml2::xml_child(run, "a:t")
  if (!is_xml_missing(text_node)) {
    return(text_node)
  }

  text_node <- shape_xml_fragment("<a:t/>")
  xml_shape_add_visual_child(run, text_node, after = "rPr")
  xml2::xml_child(run, "a:t")
}


xml_shape_mark_text_dirty <- function(runs) {
  r_pr <- xml2::xml_find_all(runs, "a:rPr")
  if (length(r_pr) > 0L) {
    xml2::xml_attr(r_pr, "dirty") <- "1"
  }
  invisible(runs)
}


xml_shape_set_background <- function(node, background = NULL) {
  if (is.null(background)) {
    return(invisible(node))
  }
  sp_pr <- xml_shape_sp_pr(node)
  xml_shape_ensure_geometry(sp_pr)
  xml_shape_remove_fill(sp_pr)
  fill <- if (identical(background, "transparent")) {
    shape_xml_fragment("<a:noFill/>")
  } else {
    shape_xml_fragment(officer::solid_fill(background))
  }
  xml_shape_add_visual_child(sp_pr, fill, after = c("xfrm", "prstGeom", "custGeom"))
  invisible(node)
}


xml_shape_set_line <- function(node, line = NULL) {
  if (is.null(line)) {
    return(invisible(node))
  }
  sp_pr <- xml_shape_sp_pr(node)
  xml_shape_ensure_geometry(sp_pr)
  xml_shape_remove_line(sp_pr)
  line_xml <- shape_xml_fragment(officer::to_pml(line))
  xml_shape_add_visual_child(
    sp_pr,
    line_xml,
    after = c("xfrm", "prstGeom", "custGeom", "noFill", "solidFill", "gradFill", "blipFill", "pattFill", "grpFill")
  )
  invisible(node)
}


xml_shape_ensure_geometry <- function(sp_pr) {
  geom <- xml2::xml_child(sp_pr, "a:prstGeom")
  custom_geom <- xml2::xml_child(sp_pr, "a:custGeom")
  if (!is_xml_missing(geom) || !is_xml_missing(custom_geom)) {
    return(invisible(sp_pr))
  }

  geom <- shape_xml_fragment('<a:prstGeom prst="rect"><a:avLst/></a:prstGeom>')
  xml_shape_add_visual_child(sp_pr, geom, after = "xfrm")
  invisible(sp_pr)
}


xml_shape_set_frame <- function(node, left = NULL, top = NULL, width = NULL,
                                height = NULL, rotation = NULL) {
  if (all(vapply(list(left, top, width, height, rotation), is.null, logical(1)))) {
    return(invisible(node))
  }
  xfrm <- xml_shape_xfrm(node)
  off <- xml_shape_xfrm_child(xfrm, "off")
  ext <- xml_shape_xfrm_child(xfrm, "ext")

  if (!is.null(left)) {
    xml2::xml_attr(off, "x") <- inch_to_emu(left)
  }
  if (!is.null(top)) {
    xml2::xml_attr(off, "y") <- inch_to_emu(top)
  }
  if (!is.null(width)) {
    xml2::xml_attr(ext, "cx") <- inch_to_emu(width)
  }
  if (!is.null(height)) {
    xml2::xml_attr(ext, "cy") <- inch_to_emu(height)
  }
  if (!is.null(rotation)) {
    xml2::xml_attr(xfrm, "rot") <- degree_to_rotation(-rotation)
  }
  invisible(node)
}


xml_shape_set_metadata <- function(node, name = NULL, description = NULL,
                                   hidden = NULL) {
  c_nv_pr <- xml_shape_c_nv_pr(node)
  if (!is.null(name)) {
    xml2::xml_attr(c_nv_pr, "name") <- name
  }
  if (!is.null(description)) {
    if (is.na(description)) {
      xml2::xml_attr(c_nv_pr, "descr") <- NULL
    } else {
      xml2::xml_attr(c_nv_pr, "descr") <- description
    }
  }
  if (!is.null(hidden)) {
    xml2::xml_attr(c_nv_pr, "hidden") <- if (hidden) "1" else "0"
  }
  invisible(node)
}


xml_shape_sp_pr <- function(node) {
  path <- switch(xml2::xml_name(node),
    sp = "p:spPr",
    pic = "p:spPr",
    cxnSp = "p:spPr",
    grpSp = "p:grpSpPr",
    NA_character_
  )
  xml2::xml_child(node, path)
}


xml_shape_remove_fill <- function(sp_pr) {
  xml_shape_remove_visual_children(sp_pr, c(
    "noFill", "solidFill", "gradFill", "blipFill", "pattFill", "grpFill"
  ))
}


xml_shape_remove_line <- function(sp_pr) {
  xml_shape_remove_visual_children(sp_pr, "ln")
}


xml_shape_remove_visual_children <- function(parent, names) {
  children <- xml2::xml_children(parent)
  remove <- xml2::xml_name(children) %in% names
  if (any(remove)) {
    xml2::xml_remove(children[remove])
  }
  invisible(parent)
}


xml_shape_add_visual_child <- function(parent, child, after) {
  children <- xml2::xml_children(parent)
  anchors <- children[xml2::xml_name(children) %in% after]
  if (length(anchors) > 0L) {
    xml2::xml_add_sibling(anchors[[length(anchors)]], child, .where = "after")
  } else {
    xml2::xml_add_child(parent, child, .where = 0)
  }
  invisible(parent)
}


xml_shape_xfrm_child <- function(xfrm, name) {
  child <- xml2::xml_child(xfrm, paste0("a:", name))
  if (!is_xml_missing(child)) {
    return(child)
  }
  fragment <- switch(name,
    off = "<a:off x=\"0\" y=\"0\"/>",
    ext = "<a:ext cx=\"0\" cy=\"0\"/>",
    cli::cli_abort("Unsupported transform child {.val {name}}.", call = NULL)
  )
  child <- shape_xml_fragment(fragment)
  if (identical(name, "off")) {
    xml2::xml_add_child(xfrm, child, .where = 0)
  } else {
    off <- xml2::xml_child(xfrm, "a:off")
    if (is_xml_missing(off)) {
      xml2::xml_add_child(xfrm, child)
    } else {
      xml2::xml_add_sibling(off, child, .where = "after")
    }
  }
  xml2::xml_child(xfrm, paste0("a:", name))
}


shape_xml_fragment <- function(x) {
  xml <- paste0(
    '<root xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" ',
    'xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main" ',
    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
    x,
    "</root>"
  )
  xml2::xml_children(xml2::read_xml(xml))[[1]]
}


inch_to_emu <- function(x) {
  as.integer(round(x * 914400))
}
