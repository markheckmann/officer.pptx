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
    src, fit = "inside", scale = 1, h_just = 0.5, v_just = 0.5,
    x_offset = 0, y_offset = 0, offset_mode = "source",
    rotation = 0, background = "transparent", line = sp_line(),
    alt = "", width = NULL, height = NULL, unit = "in") {
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

  l <- mget(names(formals()), envir = environment(), inherits = FALSE) # put all input args into list
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
    fit = img_$fit,
    h_just = img_$h_just, v_just = img_$v_just,
    x_offset = img_$x_offset, y_offset = img_$y_offset,
    offset_mode = img_$offset_mode
  )

  loc_fitted <- do.call(ph_location, f_fitted) # create new, updated ph location

  # convert to <external_img> class to use `to_pml.external_img()` for now
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


# image helpers --------------------

get_img_dimensions <- function(file) {
  ext <- tolower(file_ext(file))
  switch(ext,
    # supported by magick. See magick::magick_config()
    jpeg = get_dimensions_magick(file),
    jpg = get_dimensions_magick(file),
    png = get_dimensions_magick(file),
    gif = get_dimensions_magick(file),
    tiff = get_dimensions_magick(file),
    rsvg = get_dimensions_magick(file),
    # custom implementations
    emf = get_dimensions_emf(file),
    svg = get_dimensions_svg(file),
    pdf = get_dimensions_pdf(file),
    # webp = get_dimensions_magick(file), # must be converted first.
    cli::cli_abort("Unsupported file extension {.val .{ext}}")
  )
}


# png, jpeg, gif
get_dimensions_magick <- function(file) {
  ext <- tolower(tools::file_ext(file))
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required for xxx files.")
  }
  img  <- magick::image_read(file)
  info <- magick::image_info(img)[1, ]  # first frame for GIFs
  list(
    width  = unname(info$width),
    height = unname(info$height),
    units  = "px",
    format = ext
  )
}


get_dimensions_svg <- function(file) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required for svg files.")
  }
  img  <- magick::image_read_svg(file)
  info <- magick::image_info(img)[1, ]  # first frame for GIFs
  list(
    width  = unname(info$width),
    height = unname(info$height),
    units  = "px",
    format = "svg"
  )
}


# get dimensions
get_dimensions_pdf <- function(file, page_idx = 1) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    cli::cli_abort("Please install the {.pkg pdftools} package to handle PDFs.")
  }
  ext <- tools::file_ext(file)
  if (ext != "pdf") {
    cli::cli_abort(c(
      "{.arg file} has type {.val .{ext}}",
      "x" = "Type must be {.val .pdf}"
    ))
  }

  df_size <- pdftools::pdf_pagesize(file)
  if (nrow(df_size) > 1) {
    cli::cli_warn("PDF file has multiple pages. Using first page only.")
  }
  w_pt <- df_size$width[page_idx] # use one page_idx only
  h_pt <- df_size$height[page_idx]
  list(
    width  = w_pt,
    height = h_pt,
    # ratio  = w_pt / h_pt,
    units   = "points",
    format = "pdf"
  )
}


get_dimensions_emf <- function(file) {
  # --- EMF (millimetres) ---
  con <- file(file, "rb"); on.exit(close(con), add = TRUE)
  # Read ENHMETAHEADER fields (little-endian); base header is 88 bytes.
  iType     <- readBin(con, integer(), n = 1L, size = 4L, endian = "little")
  nSize     <- readBin(con, integer(), n = 1L, size = 4L, endian = "little")
  # rclBounds (ignored)
  invisible(readBin(con, integer(), n = 4L, size = 4L, endian = "little"))
  # rclFrame (0.01 mm units)
  frame <- readBin(con, integer(), n = 4L, size = 4L, endian = "little")
  # Optional sanity check: signature should be 0x464D4520 (" EMF")
  sig <- readBin(con, integer(), n = 1L, size = 4L, endian = "little")
  if (!identical(sig, as.integer(0x464D4520))) {
    warning("EMF signature not found; results may be unreliable.")
  }
  width_mm  <- (frame[3] - frame[1]) / 100
  height_mm <- (frame[4] - frame[2]) / 100
 list(
    width  = as.numeric(width_mm),
    height = as.numeric(height_mm),
    units  = "mm",
    format = "emf"
  )
}


get_dimensions_wmf <- function(file) {
    con <- file(path, "rb"); on.exit(close(con), add = TRUE)
    # Check for Aldus Placeable Metafile (APM) header (0x9AC6CDD7)
    key <- readBin(con, integer(), 1L, size=4L, endian="little", signed = FALSE)
    if (isTRUE(key == as.numeric(0x9AC6CDD7))) {
      # APM header present: parse and compute physical size
      invisible(readBin(con, integer(), 1L, size=2L, endian="little", signed=FALSE)) # hmf
      bbox <- readBin(con, integer(), 4L, size=2L, endian="little", signed=TRUE)     # left, top, right, bottom
      inch <- readBin(con, integer(), 1L, size=2L, endian="little", signed=FALSE)    # units per inch
      invisible(readBin(con, integer(), 1L, size=4L, endian="little", signed=FALSE)) # reserved
      invisible(readBin(con, integer(), 1L, size=2L, endian="little", signed=FALSE)) # checksum
      if (is.na(inch) || inch <= 0) stop("Invalid WMF placeable header: inch = ", inch)
      width_in  <- (bbox[3] - bbox[1]) / inch
      height_in <- (bbox[4] - bbox[2]) / inch
      return(list(
        width  = as.numeric(width_in * 25.4),
        height = as.numeric(height_in * 25.4),
        units  = "mm",
        format = "wmf",
        extra  = list(placeable_header = TRUE, units_per_inch = inch)
      ))
    }
    stop("WMF without a placeable header is not supported on this system (and magick fallback failed).")
}


# strip units (px, pt, cm, in) and convert to numeric
parse_num <- function(x) {
  as.numeric(gsub("[^0-9\\.]", "", x))
}
