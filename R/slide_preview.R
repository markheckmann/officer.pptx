# slide_preview_v1 <- function(x, slide_idx = NULL) {
#   if (!soffice_available()) {
#     cli::cli_abort("Preview requires LibreOffice to be installed and on the PATH")
#   }
#   assert_pkg_namespace("processx")
#   assert_pkg_namespace("pdftools")
#   assert_pkg_namespace("png")
#
#   stop_if_not_rpptx(x)
#   slide_idx <- slide_idx %||% x$cursor
#   stop_if_not_in_slide_range(x, slide_idx)
#
#   cli::cli_alert_info("This may take a few seconds...")
#   temp_dir <- tempdir()
#   pptx_file <- file.path(temp_dir, "temp.pptx")
#   print(x, pptx_file)
#   res <- processx::run("soffice", c(
#     "--headless",
#     "--convert-to", "pdf", pptx_file,
#     "--outdir", temp_dir
#   ))
#   if (res$status != 0) {
#     cli::cli_abort(c(
#       "Something went wrong using LibreOffice.",
#       "i" = "Could not convert PPTX to PDf"
#     ))
#   }
#
#   input_pdf <- gsub(".pptx$", ".pdf", pptx_file)
#   bitmap <- pdftools::pdf_render_page(input_pdf, page = slide_idx, dpi = 150)
#   tmp_png <- file.path(temp_dir, "temp.png")
#   png::writePNG(bitmap, target = tmp_png) # possible to save the write & read
#   img <- png::readPNG(tmp_png)
#   op <- par(mar = rep(0, 4))
#   on.exit(par(op))
#   nx <- ncol(img)
#   ny <- nrow(img)
#   plot(1, 1,
#     xlim = c(1, nx), ylim = c(0, ny),
#     type = "n", xlab = "", ylab = "", axes = TRUE, asp = 1
#   )
#   rasterImage(img, xleft = 0, ybottom = 0, xright = nx, ytop = ny)
#   rect(0, 0, ncol(img), nrow(img), border = "grey", lwd = 1)
#
#   invisible(x)
# }


#' Covert a pptx file into pdf
#' @noRd
pptx_to_pdf <- function(path_pptx, path_pdf = NULL) {
  if (!soffice_available()) {
    cli::cli_abort(c(
      "LibreOffice not found.",
      "i" = "PPTX to PDF conversion requires LibreOffice to be installed and on the PATH"
    ))
  }
  assert_pkg_namespace("processx")

  ext <- tools::file_ext(path_pptx)
  if (tolower(ext) != "pptx") {
    cli::cli_abort(c(
      "{.arg path_pptx} must be a {.val .pptx} file",
      "x" = "Found extension {.val .{ext}} instead"
    ))
  }
  if (is.character(path_pdf)) {
    ext <- tools::file_ext(path_pdf)
    if (tolower(ext) != "pdf") {
      cli::cli_abort(c(
        "{.arg path_pdf} must be a {.val .pdf} file",
        "x" = "Found extension {.val .{ext}} instead"
      ))
    }
  }

  cli::cli_alert_info("This may take a few seconds...")
  file_in <- normalizePath(path_pptx)
  temp_dir <- tempdir()
  res <- processx::run("soffice", c(
    "--headless",
    "--convert-to", "pdf", file_in,
    "--outdir", temp_dir # soffice only has outdir, no outfile
  ))
  if (res$status != 0) {
    cli::cli_abort(c(
      "Something went wrong using LibreOffice.",
      "i" = "Could not convert PPTX to PDf"
    ))
  }

  pdf_file <- gsub(".pptx$", ".pdf", basename(path_pptx))
  pdf_in_tempdir <- file.path(temp_dir, pdf_file)
  file_out <- path_pdf %||% file.path(dirname(path_pptx), pdf_file)
  file.copy(from = pdf_in_tempdir, to = file_out)
  file_out
}


#' Save slide as png
#'
#' Requires LibreOffice to be installed. Note that the conversion is quite slow.
#' In the background, the `rpptx` is first saved, converted to PDF and finally to PNG.
#'
#' @param x A `rpptx` object.
#' @param path Output path for `.png` file. If `NULL`, a temporary file is created.
#' @param slide_idx Slide number to save. Defaults to current slide.
#' @return Path to generated `.png` file.
#' @export
#' @example inst/ext/examples/example-slide-to-png.R
slide_to_png <- function(x, path = NULL, slide_idx = NULL) {
  if (!soffice_available()) {
    cli::cli_abort("Preview requires LibreOffice to be installed and on the PATH")
  }
  assert_pkg_namespace("processx")
  assert_pkg_namespace("pdftools")
  assert_pkg_namespace("png")

  stop_if_not_rpptx(x)
  slide_idx <- slide_idx %||% x$cursor
  stop_if_not_in_slide_range(x, slide_idx)

  if (is.character(path)) {
    ext <- tools::file_ext(path)
    if (tolower(ext) != "png") {
      cli::cli_abort(c(
        "{.arg path} must be a {.val .png} file",
        "x" = "Found extension {.val .{ext}} instead"
      ))
    }
  }

  temp_dir <- tempdir()
  pptx_file <- file.path(temp_dir, "temp.pptx")
  print(x, pptx_file)

  pdf_file <- pptx_to_pdf(pptx_file)

  bitmap <- pdftools::pdf_render_page(pdf_file, page = slide_idx, dpi = 150)
  png_name <- paste0("slide_", slide_idx, ".png")
  png_path <- path %||% file.path(temp_dir, png_name)
  png::writePNG(bitmap, target = png_path)
  png_path
}


#' Plot a slide preview
#'
#' Requires LibreOffice to be installed. Note that plotting is quite slow.
#' In the background, the `rpptx` is first saved, converted to PDF, then to
#' PNG and finally plotted.
#'
#' @param x A `rpptx` object.
#' @param slide_idx Slide to plot. Defaults to current slide.
#' @return Invisble `rpptx` object.
#' @export
#' @example inst/ext/examples/example-slide-preview.R
slide_preview <- function(x, slide_idx = NULL) {
  stop_if_not_rpptx(x)
  tmp_png <- slide_to_png(x, slide_idx = slide_idx)
  img <- png::readPNG(tmp_png)
  op <- graphics::par(mar = rep(0, 4))
  on.exit(graphics::par(op))
  nx <- ncol(img)
  ny <- nrow(img)
  plot(1, 1,
    xlim = c(1, nx), ylim = c(1, ny),
    type = "n", xlab = "", ylab = "", axes = TRUE, asp = 1
  )
  graphics::rasterImage(img, xleft = 0, ybottom = 0, xright = nx, ytop = ny)
  graphics::rect(0, 0, ncol(img), nrow(img), border = "grey", lwd = 1)
  invisible(x)
}
