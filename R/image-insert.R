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
                         scale = 1, h_just = 0.5, v_just = 0.5, x_offset = 0, y_offset = 0,
                         fit_inside = TRUE) {
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
        scale = scale, h_just = h_just, v_just = v_just,
        x_offset = x_offset, y_offset = y_offset, fit_inside = fit_inside
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
#' @example inst/ext/examples/example-text-replace.R
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


is_shape <- function(node) {
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


#' Scale frame to fit target frame
#'
#' @param fit_inside `[logical]`\cr If `TRUE`, scale frame to fit inside target frame.
#' If `FALSE`, scale frame, so it contains the target frame.
#' @inheritParams frame_fit_to_target
frame_scale_to_target <- function(f_source, f_target, fit_inside = TRUE) {
  assert_frame(f_source)
  assert_frame(f_target)

  scale_by_h <- f_target$height / f_source$height
  scale_by_w <- f_target$width / f_source$width

  if (frame_ratio(f_source) >= frame_ratio(f_target)) {
    scale_by <- ifelse(fit_inside, scale_by_h, scale_by_w)
  } else {
    scale_by <- ifelse(fit_inside, scale_by_w, scale_by_h)
  }
  frame_scale(f_source, scale_by)
}


#' Adjust frame so it fits into target frame
#'
#' Order of operations:
#' 1. adjust size to target frame
#' 2. scale size by factor
#' 3. justify horizontal and vertical position within target
#' 4. add x/y-offset (extra space) as multiple of image width
#'
#' @param f_source,f_target `[frame]`\cr Source and target frame.
#' @param scale `[numeric >= 0]`\cr Value to scale image by.
#' @param h_just,v_just `[numeric]`\cr Horizontal / vertical placement of image inside frame.
#' `0=`left/bottom, `1=`right/top. By default, the image is centered.
#' @param x_offset,y_offset `[numeric]`\cr Offset as multiple of image wdth/height.
#' @inheritParams frame_scale_to_target
frame_fit_to_target <- function(f_source, f_target, scale = 1, h_just = 0.5,
                                v_just = 0.5, x_offset = 0, y_offset = 0,
                                fit_inside = TRUE) {
  assert_frame(f_source)
  assert_frame(f_target)

  f_source <- frame_scale_to_target(f_source, f_target, fit_inside = fit_inside)
  f_source$left <- f_target$left
  f_source$top <- f_target$top
  f_source <- frame_scale(f_source, scale)

  f_source$left <- f_source$left + h_just * (f_target$width - f_source$width) # justify horizontally
  f_source$top <- f_source$top + (1 - v_just) * (f_target$height - f_source$height) # justify vertically
  f_source$left <- f_source$left + x_offset * f_source$width
  f_source$top <- f_source$top + y_offset * f_source$height
  f_source
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
