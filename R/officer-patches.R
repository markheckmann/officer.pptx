# patches for {officer}


#  external_image() --------------------

# external_image() requires to set the width and height which is usually
# not the way I work with officer. I just want to place an image in a placeholder,
# and expect it to fit correctly.


#' Replacement for `external_image`
#' @param src Image file path.
#' @inheritParams frame_fit_to_target
#' @param unit Unit for width and height, one of `"in"`, `"cm"`, `"mm"`.
#' @param alt Alternative text for image.
#' @param width,height Size of image file.
#' @param rotation Rotation in degrees (anti-clockwise).
#' @param background Background color (hex or color name).
#' @param line Border around image. Either a color or an [officer::sp_line] object
#'   for more control.
#' @export
#' @example inst/ext/examples/example-img.R
img <- function(
    src, scale = 1, h_just = 0.5, v_just = 0.5, x_offset = 0, y_offset = 0,
    fit_inside = TRUE, rotation = 0, background = "transparent",
    line = sp_line(), alt = "", width = NULL, height = NULL, unit = "in") {
  check_src <- all(grepl("^rId", src)) || all(file.exists(src))
  if (!check_src) {
    stop(
      "src must be a string starting with 'rId' or an existing image filename"
    )
  }

  width <- convin(unit = unit, x = width)
  height <- convin(unit = unit, x = height)

  if (!inherits(line, "sp_line") && is.color(line)) {
    line <- sp_line(color = line)
  }

  l <- mget(names(formals()), envir = environment(), inherits = FALSE)
  class(l) <- c("img")
  l
}


#' @export
print.img <- function(x, ...) {
  cli::cli_h3("<img>")
  cli::cat_line(cli::col_grey("defines where an image is placed in a placeholder"))
  utils::str(x, give.attr = FALSE, max.level = 1)
}


# update <img> object with ... args from ph_with.img
# This allows to set args in img() as well as in ph_with()
update_img_from_dots <- function(x, ...) {
  stop_if_not_class(x, "img", arg = "x")

  dots <- list(...)
  ii <- pmatch(names(dots), names(x)) # partial name matching
  ii_na <- is.na(ii)
  if (any(ii_na) > 0) {
    unknown_args <- names(dots)[ii_na]
    dots[ii_na] <- NULL
    cli::cli_warn(
      c("These args are unknown and ignored: {.val {unknown_args}}",
        "i" = "See {.fn img} for available args"
      )
    )
  }
  ii <- pmatch(names(dots), names(x))
  names(dots) <- names(x)[ii]

  # return new <img> object. Needed e.g. for `ln = sp_line(...)`
  do.call(img, utils::modifyList(x, dots, keep.null = TRUE))
}


#' Add image to placeholder
#' @inheritParams officer::ph_with
#' @export
ph_with.img <- function(x, value, location, ...) {
  stop_if_not_rpptx(x)
  location <- fortify_location(location, doc = x)
  img_ <- update_img_from_dots(value, ...)

  f_target <- frame(
    left = location$left, top = location$top,
    width = location$width, height = location$height,
    unit = "inch"
  )
  img_path <- img_$src
  f_source <- frame_from_image(img_path)
  f_fitted <- frame_fit_to_target(f_source, f_target,
    scale = img_$scale,
    h_just = img_$h_just, v_just = img_$v_just,
    x_offset = img_$x_offset, y_offset = img_$y_offset,
    fit_inside = img_$fit_inside
  )

  loc_fitted <- do.call(ph_location, f_fitted) # create new, updated ph location

  # convert to external_img class to use to_pml.external_img for now
  src <- img_$src
  class(src) <- "external_img"
  attr(src, "alt") <- img_$alt
  xml_str <- to_pml(
    x = src,
    left = loc_fitted$left,
    top = loc_fitted$top,
    width = loc_fitted$width,
    height = loc_fitted$height,
    label = location$ph_label,
    ph = location$ph,
    rot = img_$rotation,
    bg = img_$background,
    ln = img_$line
  )
  xml_node <- as_xml_document(xml_str)
  slide <- x$slide$get_slide(x$cursor)
  xml_add_child(xml_find_first(slide$get(), "//p:spTree"), xml_node)
  x
}
