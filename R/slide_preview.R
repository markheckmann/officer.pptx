#' Plot a slide preview
#'
#' Requires LibreOffice to be installed. Note that plotting is quite slow.
#' In the background, the `rpptx` is first saved, converted to PDF, then to PNG and finally plotted.
#'
#' @param x A `rpptx` object.
#' @param slide_idx Slide to plolt. Defaults to current slide.
#' @export
#' @example inst/ext/examples/example-slide-preview.R
slide_preview <- function(x, slide_idx = NULL) {
  if (!soffice_available()) {
    cli::cli_abort("Preview requires LibreOffice to be installed and on the PATH")
  }
  assert_pkg_namespace("processx")
  assert_pkg_namespace("pdftools")
  assert_pkg_namespace("png")
  requireNamespace("grid")

  officer:::stop_if_not_rpptx(x)
  slide_idx <- slide_idx %||% x$cursor
  officer:::stop_if_not_in_slide_range(x, slide_idx)

  cli::cli_alert_info("This may take a few seconds...")
  temp_dir <- tempdir()
  pptx_file <- file.path(temp_dir, "temp.pptx")
  print(x, pptx_file)
  res <- processx::run("soffice", c(
    "--headless",
    "--convert-to", "pdf", pptx_file,
    "--outdir", temp_dir
  ))
  if (res$status != 0) {
    cli::cli_abort(c(
      "Something went wrong using LibreOffice.",
      "i" = "Could not convert PPTX to PDf"
    ))
  }

  input_pdf <- gsub(".pptx$", ".pdf", pptx_file)
  bitmap <- pdftools::pdf_render_page(input_pdf, page = slide_idx, dpi = 150)
  tmp_png <- file.path(temp_dir, "temp.png")
  png::writePNG(bitmap, target = tmp_png) # possible to save the write & read
  img <- png::readPNG(tmp_png)
  op <- par(mar = rep(0, 4))
  on.exit(par(op))
  nx <- ncol(img)
  ny <- nrow(img)
  plot(1, 1,
    xlim = c(1, nx), ylim = c(0, ny),
    type = "n", xlab = "", ylab = "", axes = TRUE, asp = 1
  )
  rasterImage(img, xleft = 0, ybottom = 0, xright = nx, ytop = ny)
  rect(0, 0, ncol(img), nrow(img), border = "grey", lwd = 1)

  invisible(x)
}
