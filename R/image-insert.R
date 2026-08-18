# insert and fit image


# pptx_image_insert_at_shape <- function(x, slide_idx, ) {
#   assert_class(x, "rpptx")
#   shapes <- x |> pptx_shapes_on_slide(1, pattern = "{1}")
#   x <- xml_add_image_at_shape(x, image, shapes, scale = 1, h_just = 0, v_just = 1, x_offset = 0)
# }


# ____________----
# PPTX --------------------------------------------------


#' Insert images over selected shapes
#'
#' `image_insert()` inserts pictures over existing top-level slide objects. Targets
#' can be supplied either as a [shape_select()] selection or, for convenience, by
#' matching marker text with `pattern`.
#'
#' The original target object is kept in place. Use [shape_update()] with
#' `hidden = TRUE` if the marker should be hidden after inserting the image.
#'
#' @param x `[rpptx]` or `[pptx_shape_selection]`\cr An [officer] object. See
#'   [read_pptx()], or a selection returned by [shape_select()] for pipeline use.
#' @param image `[character]`\cr Path to image file. In selection mode, `image`
#'   must have length one or one value per selected target. In pattern mode,
#'   `image` and `pattern` must have length one or a common length.
#' @param pattern `[character]`\cr Marker text used to find target shapes. Ignored
#'   when `selection` is supplied or `x` is already a [shape_select()] result.
#' @param slide_idx `[integer]`\cr Slide indexes to search in visible presentation
#'   order. If `NULL` (default), all slides are searched. Only used with
#'   `pattern`.
#' @param fixed `[logical]`\cr Interpret `pattern` as fixed substring? If `FALSE`,
#'   `pattern` is interpreted as a regular expression.
#' @param rotation `[numeric]`\cr Rotation in degrees (anti-clockwise). Default
#'   `0`.
#' @param background `[character]`\cr Background color behind the image (hex or
#'   color name). Default `"transparent"`.
#' @param line Border around the image. Either a color string or an
#'   [officer::sp_line()] object for detailed control. Default: no border.
#' @param alt `[character]`\cr Alternative text for accessibility. Default `""`.
#' @param selection `[pptx_shape_selection]`\cr A selection returned by
#'   [shape_select()]. Leave `NULL` when using `pattern` or when `x` is already a
#'   piped selection.
#' @param empty `[character(1)]`\cr What to do when no target shape is selected.
#'   The default, `"error"`, fails early. Use `"ignore"` to return `x`
#'   unchanged.
#' @inheritParams frame_fit_to_target
#' @return The modified `rpptx` object.
#' @export
#' @example inst/ext/examples/example-image-insert.R
image_insert <- function(x, image, pattern = NULL, slide_idx = NULL, fixed = TRUE,
                         fit = "inside", scale = 1,
                         h_just = 0.5, v_just = 0.5,
                         x_offset = 0, y_offset = 0, offset_mode = "source",
                         rotation = 0, background = "transparent",
                         line = NULL, alt = "",
                         selection = NULL, empty = c("error", "ignore")) {
  empty <- match.arg(empty)
  line <- image_insert_normalize_line(line)
  inputs <- image_insert_inputs(x, selection)
  x <- inputs$x
  selection <- inputs$selection

  if (!is.null(selection) && !is.null(pattern)) {
    cli::cli_abort(
      "Supply either {.arg selection} or {.arg pattern}, not both.",
      call = NULL
    )
  }

  if (is.null(selection)) {
    selection <- image_insert_pattern_selection(
      x = x,
      image = image,
      pattern = pattern,
      slide_idx = slide_idx,
      fixed = fixed,
      empty = empty
    )
    image <- selection$image
    selection$image <- NULL
  } else {
    selection <- shape_selection_validate(x, selection, empty = empty)
    image <- image_insert_recycle_image(image, nrow(selection))
  }

  if (nrow(selection) == 0L) {
    return(x)
  }

  image_insert_validate_selection(selection)
  frames <- image_insert_frames(
    image = image,
    selection = selection,
    fit = fit,
    scale = scale,
    h_just = h_just,
    v_just = v_just,
    x_offset = x_offset,
    y_offset = y_offset,
    offset_mode = offset_mode
  )

  old_cursor <- x$cursor
  on.exit(x$cursor <- old_cursor, add = TRUE)

  for (i in seq_len(nrow(selection))) {
    xml_add_image_after_shape(
      shape = selection$node[[i]],
      img_path = image[[i]],
      frame = frames[[i]],
      rotation = rotation,
      background = background,
      line = line,
      alt = alt
    )
  }

  x
}


#' Insert image at shape position
#'
#' @param x `[rpptx]`\cr An [officer] object. See [read_pptx()].
#' @param image `[character]`\cr Path to image file.
#' @param pattern `[character]`\cr Vector with patterns to find shapes. Regex is not interpreted.
#' @param slide_idx `[numeric]`\cr Index of slides to process. If `NULL` (default), all slides
#'   are processed.
#' @inheritParams frame_fit_to_target
#' @export
#' @keywords internal
pptx_image_insert_at_shape_temp <- function(x, image, pattern, slide_idx = NULL,
                                            scale = 1, h_just = 0.5,
                                            v_just = 0.5, x_offset = 0, y_offset = 0,
                                            fit_inside = TRUE) {
  .Deprecated("image_insert", old = "pptx_image_insert_at_shape_temp")
  fit <- if (isTRUE(fit_inside)) "inside" else "outside"
  image_insert(
    x = x,
    image = image,
    pattern = pattern,
    slide_idx = slide_idx,
    fixed = TRUE,
    fit = fit,
    scale = scale,
    h_just = h_just,
    v_just = v_just,
    x_offset = x_offset,
    y_offset = y_offset,
    empty = "ignore"
  )
}


image_insert_inputs <- function(x, selection = NULL) {
  if (inherits(x, "pptx_shape_selection")) {
    if (!is.null(selection)) {
      cli::cli_abort(
        "Do not supply {.arg selection} when piping a {.cls pptx_shape_selection} into {.fn image_insert}.",
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


image_insert_pattern_selection <- function(x, image, pattern, slide_idx, fixed, empty) {
  if (is.null(pattern)) {
    cli::cli_abort(
      "Supply {.arg pattern} or {.arg selection}.",
      call = NULL
    )
  }
  if (!is.logical(fixed) || length(fixed) != 1L || is.na(fixed)) {
    cli::cli_abort("{.arg fixed} must be `TRUE` or `FALSE`.", call = NULL)
  }

  n <- image_insert_common_size(
    image = image_insert_value_size(image, "image"),
    pattern = image_insert_value_size(pattern, "pattern")
  )
  image <- rep_len(as.character(image), n)
  pattern <- rep_len(as.character(pattern), n)
  match <- if (fixed) "contains" else "regex"

  rows <- vector("list", n)
  for (i in seq_len(n)) {
    selection <- shape_select(
      x,
      slide_idx = slide_idx,
      text = pattern[[i]],
      match = match
    )
    if (nrow(selection) == 0L && empty == "error") {
      cli::cli_abort(
        "No shape matched {.arg pattern} {.val {pattern[[i]]}}.",
        call = NULL
      )
    }
    selection$image <- rep(image[[i]], nrow(selection))
    rows[[i]] <- selection
  }

  out <- dplyr::bind_rows(rows)
  new_pptx_shape_selection(out, source = shape_selection_source(x, out), pptx = x)
}


image_insert_recycle_image <- function(image, n) {
  if (n == 0L) {
    return(character())
  }
  size <- image_insert_value_size(image, "image")
  if (!size %in% c(1L, n)) {
    cli::cli_abort(
      "{.arg image} must have length 1 or the number of selected shapes ({n}).",
      call = NULL
    )
  }
  rep_len(as.character(image), n)
}


image_insert_value_size <- function(x, arg) {
  if (!is.character(x) || anyNA(x)) {
    cli::cli_abort("{.arg {arg}} must be a character vector without missing values.", call = NULL)
  }
  size <- length(x)
  if (size == 0L) {
    cli::cli_abort("{.arg {arg}} must not be empty.", call = NULL)
  }
  size
}


image_insert_common_size <- function(...) {
  sizes <- unlist(list(...), use.names = FALSE)
  n <- max(sizes)
  invalid <- unique(sizes[sizes != 1L & sizes != n])
  if (length(invalid) > 0L) {
    cli::cli_abort(
      "{.arg image} and {.arg pattern} must have length 1 or a common length.",
      call = NULL
    )
  }
  n
}


image_insert_normalize_line <- function(line) {
  if (is.null(line)) {
    return(NULL)
  }
  if (inherits(line, "sp_line")) {
    return(line)
  }
  if (is.character(line) && length(line) == 1L && is.color(line)) {
    return(officer::sp_line(color = line))
  }
  cli::cli_abort(
    "{.arg line} must be `NULL`, a color string, or an {.fn sp_line} object.",
    call = NULL
  )
}


image_insert_validate_selection <- function(selection) {
  if (any(selection$placeholder)) {
    cli::cli_abort(
      c(
        "Selected targets must not be PowerPoint placeholders.",
        "i" = "Use ordinary slide shapes as image markers."
      ),
      call = NULL
    )
  }

  geometry <- selection[c("left", "top", "width", "height")]
  valid <- stats::complete.cases(geometry) &
    is.finite(selection$left) &
    is.finite(selection$top) &
    is.finite(selection$width) &
    is.finite(selection$height) &
    selection$width > 0 &
    selection$height > 0
  if (!all(valid)) {
    cli::cli_abort(
      "Selected targets must have finite positive geometry.",
      call = NULL
    )
  }
}


image_insert_frames <- function(image, selection, fit, scale, h_just, v_just,
                                x_offset, y_offset, offset_mode) {
  frames <- vector("list", nrow(selection))
  for (i in seq_len(nrow(selection))) {
    f_source <- frame_from_image(image[[i]])
    f_target <- frame(
      left = selection$left[[i]],
      top = selection$top[[i]],
      width = selection$width[[i]],
      height = selection$height[[i]],
      unit = "in"
    )
    frames[[i]] <- frame_fit_to_target(
      f_source = f_source,
      f_target = f_target,
      fit = fit,
      scale = scale,
      h_just = h_just,
      v_just = v_just,
      x_offset = x_offset,
      y_offset = y_offset,
      offset_mode = offset_mode
    )
  }
  frames
}


xml_add_image_after_shape <- function(shape, img_path, frame, rotation = 0,
                                      background = "transparent", line = NULL,
                                      alt = "") {
  assert_node(shape, argname = "shape")
  assert_frame(frame)

  line <- line %||% officer::sp_line(lwd = 0)
  image <- officer::external_img(img_path)
  attr(image, "alt") <- alt
  xml <- officer::to_pml(
    x = image,
    left = frame$left,
    top = frame$top,
    width = frame$width,
    height = frame$height,
    label = basename(img_path),
    ph = "",
    rot = rotation,
    bg = background,
    ln = line
  )
  node <- xml2::read_xml(xml)
  xml2::xml_add_sibling(shape, node, .where = "after")
  invisible(node)
}


xml_add_image_at_shape <- function(x, img_path, shape, ...) {
  assert_class(x, "rpptx")
  if (is_node(shape)) {
    shape <- list(shape)
  }
  n <- max(length(img_path), length(shape))
  img_path <- rep_len(img_path, n)
  shape <- rep_len(shape, n)

  for (i in seq_len(n)) {
    f_target <- xml_shape_get_frame(shape[[i]])
    f_source <- frame_from_image(img_path[[i]])
    f_fitted <- frame_fit_to_target(f_source, f_target, ...)
    xml_add_image_after_shape(shape[[i]], img_path[[i]], f_fitted)
  }
  x
}


# ____________----
# FRAME --------------------------------------------------


#' Frame class to hold position info
#' @param left,top,width,height `[numeric >= 0]`\cr Self explanatory.
#' @param unit `[character]`\cr Not yet used.
#' @keywords internal
#' @export
frame <- function(left, top, width, height, unit = NA) {
  x <- list(
    left = as.numeric(left),
    top = as.numeric(top),
    width = as.numeric(width),
    height = as.numeric(height),
    unit = unit
  )
  class(x) <- c("frame", "list")
  x
}


#' @export
#' @keywords internal
print.frame <- function(x, ...) {
  msg <- glue::glue("<frame left:{x$left} top:{x$top} width:{x$width} height:{x$height} unit:{x$unit}>")
  cli::cat_line(msg)
}


#' Frame to character
#' @export
#' @keywords internal
#' @param sep Separator between coords.
#' @param digits Digits to round to.
as.character.frame <- function(x, ..., sep = " ", digits = 2) {
  l <- x[c("left", "top", "width", "height")]
  l <- lapply(l, round, digits = digits)
  msg <- glue::glue("left:{l$left}{sep}top:{l$top}{sep}width:{l$width}{sep}height:{l$height}")
}


# f <- frame(1,2,3,4)
# is_frame(f)
is_frame <- function(x) {
  inherits(x, "frame")
}


# f <- frame(1,2,3,4)
# assert_frame(f)
#
# assert_frame(1) # fails
# assert_frame("a") # fails
assert_frame <- function(x) {
  var_name <- rlang::enquo(x) |> rlang::quo_name()
  if (!is_frame(x)) {
    cli::cli_abort("{.arg {var_name}} must be a {.val frame} object. Got {.val {class(x)}}")
  }
}


frame_scale <- function(x, factor = 1) {
  assert_frame(x)
  x$width <- x$width * factor
  x$height <- x$height * factor
  x
}


frame_ratio <- function(x) {
  assert_frame(x)
  x$height / x$width
}


# frame_from_image <- function(path) {
#   img <- read_image(path)
#   frame(left = 0, top = 0, width = ncol(img), height = nrow(img))
# }


frame_from_image <- function(path) {
  img <- get_img_dimensions(path)
  frame(left = 0, top = 0, width = img$width, height = img$height)
}


#' Scale frame to size of target frame
#'
#' *Note*: Scaling only, no translation (x,y) happening here.
#'
#' @param fit `[character]`\cr One of `inside`: fit inside target frame,
#'   `outside`: fit around target frame, `width`: match target frame width,
#'   `height`: match target frame height, `fill`: stretch to target frame,
#'   `none`: no scaling.
#' @inheritParams frame_fit_to_target
#' @keywords internal
#' @export
#' @returns A `frame` object.
frame_scale_to_target <- function(f_source, f_target, fit = "inside") {
  assert_frame(f_source)
  assert_frame(f_target)

  fit <- match.arg(tolower(fit), c("inside", "outside", "width", "height", "fill", "none"))

  scale_by_h <- f_target$height / f_source$height
  scale_by_w <- f_target$width / f_source$width
  f_ratio <- frame_ratio(f_source) / frame_ratio(f_target)

  if (fit == "none") {
    return(f_source)
  }
  if (fit == "fill") {
    f_source$width <- f_target$width
    f_source$height <- f_target$height
    return(f_source)
  }
  scale_by <- switch(fit,
    width = scale_by_w,
    height = scale_by_h,
    inside = ifelse(f_ratio >= 1, scale_by_h, scale_by_w),
    outside = ifelse(f_ratio >= 1, scale_by_w, scale_by_h),
  )
  frame_scale(f_source, scale_by)
}


#' Match center of source to center of target frame
#'
#' Does x and y translation to match centers
#'
#' @inheritParams frame_fit_to_target
#' @keywords internal
#' @export
#' @returns A `frame` object.
frame_center_to_target <- function(f_source, f_target) {
  assert_frame(f_source)
  assert_frame(f_target)
  f_source$left <- f_target$left + f_target$width / 2 - f_source$width / 2
  f_source$top <- f_target$top + f_target$height / 2 - f_source$height / 2
  f_source
}


#' Adjust frame so it fits to target frame
#'
#' Order of operations (same as order of args):
#' 1. Adjust source size to target frame and move to match target's center
#' 2. Scale by factor
#' 3. Justify horizontal and vertical position within target
#' 4. Add x/y-offset (extra space) as multiple of scaled image width/height
#'
#' @param f_source,f_target `[frame]`\cr Source and target frame.
#' @param scale `[numeric >= 0]`\cr Value to scale image by.
#' @param h_just,v_just `[numeric]`\cr Horizontal / vertical placement of image inside frame.
#' `0=`left/bottom, `1=`right/top. By default, the image is centered.
#' @param x_offset,y_offset `[numeric]`\cr Offset as multiple of scaled source frame width / height (default) or raw
#' units (see `offset_mode`).
#' @param offset_mode `[character]`\cr Offset values are interpreted as one of: multiple of scaled source frame's
#' width / height (`source`, default), multiple of target frame's width / height (`target`), or raw `units`.
#' @inheritParams frame_scale_to_target
#' @keywords internal
#' @export
#' @returns A `frame` object.
frame_fit_to_target <- function(f_source, f_target,
                                fit = "inside", scale = 1,
                                h_just = 0.5, v_just = 0.5,
                                x_offset = 0, y_offset = 0, offset_mode = "source") {
  assert_frame(f_source)
  assert_frame(f_target)
  offset_mode <- match.arg(tolower(offset_mode), c("source", "target", "units"))
  frame_fit_validate_numeric(scale, "scale", lower = 0)
  frame_fit_validate_numeric(h_just, "h_just")
  frame_fit_validate_numeric(v_just, "v_just")
  frame_fit_validate_numeric(x_offset, "x_offset")
  frame_fit_validate_numeric(y_offset, "y_offset")

  .f_source <- f_source
  # browser()
  # scaling only. no change in top/left yet
  f_source <- frame_scale_to_target(f_source, f_target, fit = fit)
  f_source <- frame_scale(f_source, scale)

  # place source over center of target to start from (i.e. h_just = .5, v_just = .5)
  f_source <- frame_center_to_target(f_source, f_target)

  # h_just, v_just: Must work also if source frame is bigger than target
  w_delta <- (f_target$width - f_source$width)
  h_delta <- (f_target$height - f_source$height)
  f_source$left <- f_source$left + (h_just - .5) * w_delta
  f_source$top <- f_source$top - (v_just - .5) * h_delta

  # x_offset, y_offset (multiple of scaled source w/h, target w/h, or raw units)
  if (offset_mode == "source") {
    f_source$left <- f_source$left + (x_offset * f_source$width)
    f_source$top <- f_source$top - (y_offset * f_source$height)
  } else if (offset_mode == "target") {
    f_source$left <- f_source$left + (x_offset * f_target$width)
    f_source$top <- f_source$top - (y_offset * f_target$height)
  } else if (offset_mode == "units") {
    f_source$left <- f_source$left + x_offset
    f_source$top <- f_source$top - y_offset
  }
  f_source
}


frame_fit_validate_numeric <- function(x, arg, lower = NULL) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    cli::cli_abort("{.arg {arg}} must be a finite number.", call = NULL)
  }
  if (!is.null(lower) && x < lower) {
    cli::cli_abort("{.arg {arg}} must be greater than or equal to {lower}.", call = NULL)
  }
  invisible(x)
}


frame_fit_to_target_v1 <- function(f_source, f_target, fit = "inside", scale = 1, h_just = 0.5, v_just = 0.5,
                                   x_offset = 0, y_offset = 0) {
  assert_frame(f_source)
  assert_frame(f_target)
  fit <- match.arg(tolower(fit), c("inside", "outside", "width", "height", "none"))

  f_source <- frame_scale_to_target(f_source, f_target, fit = fit) # scaled, not changing top/left
  f_source$left <- f_target$left
  f_source$top <- f_target$top
  f_source <- frame_scale(f_source, scale)

  f_source$left <- f_source$left + h_just * (f_target$width - f_source$width) # justify horizontally
  f_source$top <- f_source$top + (1 - v_just) * (f_target$height - f_source$height) # justify vertically
  f_source$left <- f_source$left + x_offset * f_source$width
  f_source$top <- f_source$top + y_offset * f_source$height
  f_source
}


# add bottom and right coords to frame
frame_add_bottom_right <- function(frame) {
  assert_frame(frame)
  frame$bottom <- frame$top + frame$height
  frame$right <- frame$left + frame$width
  frame
}


# plot rectangle at frame coords
frame_plot_rect <- function(frame, label = "<frame>", color = "black", coords = TRUE, ...) {
  assert_frame(frame)
  dots <- list(...)
  graphics::rect(
    xleft = frame$left, xright = frame$right, ytop = frame$top, ybottom = frame$bottom,
    border = color, col = scales::alpha(color, alpha = .1), ...
  )
  graphics::text(x = frame$right, y = frame$top, label = label, col = color, adj = c(1, 1), font = 2)
  if (coords) {
    graphics::text(
      x = frame$right, y = frame$top - 1.5 * graphics::strheight("A"),
      label = as.character(frame, sep = "\n"), col = color, adj = c(1, 1), cex = .75
    )
  }
}


# draw two frames
# example:
# layout(matrix(1:2, ncol = 2))
# f1 <- frame(left = 0, top = 0, width = 1, height = 1)
# f2 <- frame(left = 0, top = 0, width = 2, height = 3)
# frames_draw(f1, f2)
# f1_fitted <- frame_fit_to_target(f1, f2)
# frames_draw(f1_fitted, f2)
#
frames_draw <- function(frame_1, frame_2, labels = c("source", "target"),
                        colors = c("darkgreen", "blue"),
                        title = "Frames", coords = FALSE, xlim = NULL, ylim = NULL) {
  frame_1 <- frame_add_bottom_right(frame_1)
  frame_2 <- frame_add_bottom_right(frame_2)


  top <- min(frame_1$top, frame_2$top)
  bottom <- max(frame_1$bottom, frame_2$bottom)
  left <- min(frame_1$left, frame_2$left)
  right <- max(frame_1$right, frame_2$right)

  expand <- 1.05
  xlim <- xlim %||% c(left, right) * expand
  ylim <- sort(ylim, decreasing = TRUE) %||% c(bottom, top) * expand # axis is inverted, higher values towwards bottom
  plot(0, xlim = xlim, ylim = ylim, type = "n", yaxt = "n", xaxt = "n", xlab = "", ylab = "", frame = FALSE, asp = 1)
  # browser()
  # plot(0, xlim = xlim, ylim = ylim, xlab = "", ylab = "", frame = T, asp = 1)
  graphics::axis(1, col = "grey50", col.axis = "grey50")
  graphics::axis(2, col = "grey50", col.axis = "grey50", las = 1, at = pretty(ylim))

  frame_plot_rect(frame_1, color = colors[1], lty = 1, label = labels[1], coords = coords)
  frame_plot_rect(frame_2, color = colors[2], lty = 2, label = labels[2], coords = coords)
  title(title)
}

# frames_draw <- function(frame_1, frame_2, labels = c("source", "target"),
#                         colors = c("darkgreen", "blue"),
#                         title = "Frames", coords = FALSE, xlim = NULL, ylim = NULL) {
#   frame_1 <- frame_add_bottom_right(frame_1)
#   frame_2 <- frame_add_bottom_right(frame_2)
#
#   top <- max(frame_1$top, frame_2$top)
#   bottom <- min(frame_1$bottom, frame_2$bottom)
#   left <- min(frame_1$left, frame_2$left)
#   right <- max(frame_1$right, frame_2$right)
#
#   expand <- 1.05
#   xlim <- xlim %||% c(left, right) * expand
#   ylim <- ylim %||% c(bottom, top) * expand
#   plot(0, xlim = xlim, ylim = ylim, type = "n", yaxt = "n", xaxt = "n", xlab = "", ylab = "", frame = FALSE, asp = 1)
#
#   axis(1, col = "grey50", col.axis = "grey50", )
#   axis(2, col = "grey50", col.axis = "grey50", las = 1)
#
#   frame_plot_rect(frame_1, color = colors[1], lty = 1, label = labels[1], coords = coords)
#   frame_plot_rect(frame_2, color = colors[2], lty = 2, label = labels[2], coords = coords)
#   title(title)
# }


# ____________----
# HELPERS --------------------------------------------------


#' Read image
#'
#' The reader function depends on the image file format.
#' It can process `jpeg` and `png`.
#' @param path `[]` Path to image file.
#' @keywords internal
read_image <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("File does not exist: {.file {path}}")
  }
  ext <- tools::file_ext(path)
  ext_allowed <- c("jpg", "jpeg", "png")
  if (!ext %in% ext_allowed) {
    cli::cli_abort("File format {.val .{ext}} is not allowed. Only {.val {ext_allowed}}")
  }
  reader <- switch(ext,
    jpg = jpeg::readJPEG,
    jpeg = jpeg::readJPEG,
    png = png::readPNG
  )
  reader(path)
}


is_node <- function(node) {
  inherits(node, "xml_node")
}


assert_node <- \(x, argname = NULL) {
  argname <- argname %||% rlang::as_name(rlang::ensym(x))
  if (!is_node(x)) {
    cli::cli_abort("{.arg {argname}} must be an {.cls xml_node} node, not {.cls {class(x)[1]}}")
  }
}


assert_xml_shapetree <- \(x, argname = NULL) {
  argname <- argname %||% rlang::as_name(rlang::ensym(x))
  if (!xml_is_shapetree(x)) {
    cli::cli_abort("{.arg {argname}} must be an {.cls xml_node} node, not {.cls {class(x)[1]}}")
  }
}


xml_is_shapetree <- \(x) {
  if (!is_node(x)) {
    return(FALSE)
  }
  xml2::xml_name(x) == "spTree"
}
