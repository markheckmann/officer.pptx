# insert and fit image


# pptx_image_insert_at_shape <- function(x, slide_idx, ) {
#   assert_class(x, "rpptx")
#   shapes <- x |> pptx_shapes_on_slide(1, pattern = "{1}")
#   x <- xml_add_image_at_shape(x, image, shapes, scale = 1, h_just = 0, v_just = 1, x_offset = 0)
# }


# ____________----
# PPTX --------------------------------------------------


#' Insert image at shape position
#'
#' @param x `[rpptx]`\cr An [officer] object. See [read_pptx()].
#' @param image `[character]`\cr Path to image file.
#' @param pattern `[character]`\cr Vector with patterns to find shapes. Interpreted as regex.
#' Wrap in [stringr::fixed] for fixed string object or set `fixed = TRUE`.
#' @param slide_idx `[numeric]`\cr Index of slides to process. If `NULL` (default), all slides
#'   are processed.
#' @param fixed `[logical]`\cr Interpret `pattern` as fixed string? (default `TRUE`).
#' @inheritParams frame_fit_to_target
#' @export
#' @example inst/ext/examples/example-text-replace.R
image_insert <- function(x, image, pattern, slide_idx = NULL, fixed = TRUE,
                         fit = "inside", scale = 1,
                         h_just = 0.5, v_just = 0.5,
                         x_offset = 0, y_offset = 0, offset_mode = "source") {
  assert_class(x, "rpptx")
  slide_idx <- slide_idx %||% seq_along(x)
  if (fixed) {
    pattern <- stringr::fixed(pattern)
  }
  ii <- seq_along(image)
  for (s_idx in slide_idx) {
    x$cursor <- s_idx # necessary as used by
    for (i in ii) {
      cat("\rs_idx:", s_idx, "i:", i, "image:", basename(image[i]), "pattern", pattern[i])
      .pptx_image_insert_at_shape_on_slide(x,
        image = image[i], pattern = pattern[i], slide_idx = s_idx,
        fit = fit, scale = scale, h_just = h_just, v_just = v_just,
        x_offset = x_offset, y_offset = y_offset, offset_mode = offset_mode
      )
      # shapes <- x |> pptx_shapes_on_slide(slide_idx = idx, pattern = pattern[i])
      # x <- xml_add_image_at_shape(x, image[i], shapes, )
    }
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
  .Deprecated("insert_image", old = "pptx_image_insert_at_shape_temp")
  assert_class(x, "rpptx")
  stopifnot(length(image) == length(pattern))
  pattern <- stringr::fixed(pattern) # currently not regex
  slide_idx <- slide_idx %||% seq_along(x)
  ii <- seq_along(image)
  for (s_idx in slide_idx) {
    x$cursor <- s_idx # necessary as used by
    for (i in ii) {
      cat("\rs_idx:", s_idx, "i:", i, "image:", basename(image[i]), "pattern", pattern[i])
      .pptx_image_insert_at_shape_on_slide(x,
        image = image[i], pattern = pattern[i], slide_idx = s_idx,
        scale = scale, h_just = h_just, v_just = v_just,
        x_offset = x_offset, y_offset = y_offset, fit_inside = fit_inside
      )
      # shapes <- x |> pptx_shapes_on_slide(slide_idx = idx, pattern = pattern[i])
      # x <- xml_add_image_at_shape(x, image[i], shapes, )
    }
  }
  x
}


# Insert image in placeholder shape
.pptx_image_insert_at_shape_on_slide <- function(x, image, pattern, slide_idx, ...) {
  shapes <- x |> pptx_shapes_on_slide(slide_idx, pattern = pattern)
  if (length(shapes) > 0) {
    xml_add_image_at_shape(x, image, shapes, ...)
  }
}


.xml_add_image_at_shape <- function(x, img_path, shape, ...) {
  if (!is_node(shape)) {
    cli::cli_abort(
      c("Incorrect class for {.arg shape}",
        "i" = "{.arg shape} must have class {.cls <xml_node>}, but found {.cls {class(shape)[1]}}"
      )
    )
  }
  if (!xml_name(shape) %in% c("sp", "cxnSp")) {
    cli::cli_abort(
      c("incorrect type for {.arg shape}",
        "i" = "{.arg shape} requires be a shape shape but got {.val {xml_name(shape)}}"
      )
    )
  }
  if (xml_is_placeholder(shape)) {
    cli::cli_abort(
      c("{.arg shape} is a placeholder",
        "i" = "Placeholder shapes are not supported."
      )
    )
  }
  f_target <- xml_shape_get_frame(shape)
  f_source <- frame_from_image(img_path)
  f_fitted <- frame_fit_to_target(f_source, f_target, ...)
  loc <- do.call(ph_location, f_fitted)
  ph_with(x, external_img(img_path), location = loc)
}


# Caveat: Only works for non placeholder shapes.
xml_add_image_at_shape <- function(x, img_path, shape, ...) {
  if (is_node(shape)) {
    shape <- list(shape)
  }
  n <- max(length(img_path), length(shape))
  img_path <- rep_len(img_path, n)
  shape <- rep_len(shape, n)
  for (i in seq(n)) {
    .xml_add_image_at_shape(x, img_path[[i]], shape[[i]], ...)
  }
  x
}


xml_is_shape <- function(node) {
  xml_name(node) %in% c("sp", "cxnSp")
}


# ____________----
# FRAME --------------------------------------------------


#' Frame class to fold position info
#' @param left,top,width,height `[numeric >= 0]`\cr Self explanatory.
#' @param unit `[character]`\cr Not yet used.
#' @keywords internal
frame <- function(left, top, width, height, unit = NA) {
  x <- list(
    left = as.numeric(left),
    top = as.numeric(top),
    width = as.numeric(width), height = as.numeric(height), unit = unit
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
#' @param digitsDigiuts to round to.
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


frame_from_image <- function(path) {
  img <- read_image(path)
  frame(left = 0, top = 0, width = ncol(img), height = nrow(img))
}


#' Scale frame to size of target frame
#'
#' *Note*: Scaling only, no translation (x,y) happening here.
#'
#' @param fit `[character]`\cr One of `inside`: fit inside target frame, `outside`: fit around target frame,
#' `width`: match target frame width, `height`: match target frame height, `none`: no scaling.
#' If `FALSE`, scale frame, so it contains the target frame.
#' @inheritParams frame_fit_to_target
#' @keywords internal
#' @export
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


#' Adjust frame so it fits to target frame
#'
#' Order of operations (same as order of args):
#' 1. Adjust source size to target frame
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
#' @param offset_mode `[character]`\cr Offset values are interprete as one of: multiple of scaled source frame's
#' width / height (`source`, default), multiple of target frame's width / height (`target`), or raw `units`.
#' @inheritParams frame_scale_to_target
#' @keywords internal
frame_fit_to_target <- function(f_source, f_target,
                                fit = "inside", scale = 1,
                                h_just = 0.5, v_just = 0.5,
                                x_offset = 0, y_offset = 0, offset_mode = "source") {
  assert_frame(f_source)
  assert_frame(f_target)
  fit <- match.arg(tolower(fit), c("inside", "outside", "width", "height", "none"))
  offset_mode <- match.arg(tolower(offset_mode), c("source", "target", "units"))

  # scaling only. no change in top/left yet
  f_source <- frame_scale_to_target(f_source, f_target, fit = fit)
  f_source <- frame_scale(f_source, scale)

  # place source over center of target to start from (i.e. h_just = .5, v_just = .5)
  f_source$left <- f_target$left + (f_target$width) / 2 - f_source$width / 2
  f_source$top <- f_target$top - f_target$height / 2 + f_source$height / 2

  # h_just, v_just: Must work also if source frame is bigger than target
  w_delta <- (f_target$width - f_source$width)
  h_delta <- (f_target$height - f_source$height)
  f_source$left <- f_source$left + (h_just - .5) * w_delta
  f_source$top <- f_source$top + (v_just - .5) * h_delta

  # x_offset, y_offset (multiple of scaled source w/h, target w/h, or raw units)
  if (offset_mode == "source") {
    f_source$left <- f_source$left + (x_offset * f_source$width)
    f_source$top <- f_source$top + (y_offset * f_source$height)
  } else if (offset_mode == "target") {
    f_source$left <- f_source$left + (x_offset * f_target$width)
    f_source$top <- f_source$top + (y_offset * f_target$height)
  } else if (offset_mode == "units") {
    f_source$left <- f_source$left + x_offset
    f_source$top <- f_source$top + y_offset
  }
  f_source
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
  frame$bottom <- frame$top - frame$height
  frame$right <- frame$left + frame$width
  frame
}


# plot rectangle at frame coords
frame_plot_rect <- function(frame, label = "<frame>", color = "black", coords = TRUE, ...) {
  assert_frame(frame)
  dots <- list(...)
  rect(
    xleft = frame$left, xright = frame$right, ytop = frame$top, ybottom = frame$bottom,
    border = color, col = scales::alpha(color, alpha = .1), ...
  )
  text(x = frame$right, y = frame$top, label = label, col = color, adj = c(1, 1), font = 2)
  if (coords) {
    text(
      x = frame$right, y = frame$top - 1.5 * strheight("A"),
      label = as.character(frame, sep = "\n"), col = color, adj = c(1, 1), cex = .75
    )
  }
}


# draw two frames
# example:
# f1 <- frame(0, 0, 1, 1)
# f2 <- frame(0, 0, 2, 4)
# frames_draw(f1, f2)
# f1_fitted <- frame_fit_to_target(f1, f2)
# frames_draw(f1_fitted, f2)
#
frames_draw <- function(frame_1, frame_2, labels = c("source", "target"), colors = c("darkgreen", "blue"),
                        title = "Frames", coords = TRUE, xlim = NULL, ylim = NULL) {
  frame_1 <- frame_add_bottom_right(frame_1)
  frame_2 <- frame_add_bottom_right(frame_2)

  top <- max(frame_1$top, frame_2$top)
  bottom <- min(frame_1$bottom, frame_2$bottom)
  left <- min(frame_1$left, frame_2$left)
  right <- max(frame_1$right, frame_2$right)


  expand <- 1.05
  xlim <- xlim %||% c(left, right) * expand
  ylim <- ylim %||% c(bottom, top) * expand
  plot(0, xlim = xlim, ylim = ylim, type = "n", yaxt = "n", xaxt = "n", xlab = "", ylab = "", frame = FALSE, asp = 1)

  axis(1, col = "grey50", col.axis = "grey50", )
  axis(2, col = "grey50", col.axis = "grey50", las = 1)

  frame_plot_rect(frame_1, color = colors[1], lty = 1, label = labels[1], coords = coords)
  frame_plot_rect(frame_2, color = colors[2], lty = 2, label = labels[2], coords = coords)
  title(title)
}


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
