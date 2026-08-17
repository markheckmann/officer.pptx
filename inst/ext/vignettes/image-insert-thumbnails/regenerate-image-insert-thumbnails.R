#!/usr/bin/env Rscript

# Regenerate thumbnails used by vignettes/image-insert.qmd.
# Run from the package root.
# Requires LibreOffice, magick, and the local package sources.

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
} else {
  stop("Package 'devtools' is required to load the local package sources.", call. = FALSE)
}

if (!requireNamespace("magick", quietly = TRUE)) {
  stop("Package 'magick' is required.", call. = FALSE)
}

out_dir <- "inst/ext/vignettes/image-insert-thumbnails"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

border_color <- "#878C96"

save_thumb <- function(deck, slide_idx = 1, name) {
  pptx_file <- tempfile(fileext = ".pptx")
  print(deck, target = pptx_file)

  temp_dir <- tempdir()
  res <- processx::run("soffice", c(
    "--headless",
    "--convert-to", "png",
    pptx_file,
    "--outdir", temp_dir
  ), timeout = 60)
  png_actual <- sub("\\.pptx$", ".png", pptx_file)
  if (!file.exists(png_actual)) {
    stop("LibreOffice PNG conversion failed for: ", name, call. = FALSE)
  }

  img <- magick::image_read(png_actual)
  img <- magick::image_border(img, border_color, "3x3")
  out <- file.path(out_dir, name)
  magick::image_write(img, out)
  message("Wrote ", out)
  invisible(out)
}

img_path <- system.file("ext/images/dog_1.jpg", package = "officer.pptx")

# --- Thumbnail 1: Before (marker shapes) ---
x <- read_pptx()
x <- add_slide(x, layout = "Blank")
x <- shape_add(
  x,
  left = c(0.5, 5.2),
  top = c(1, 1),
  width = c(4, 4),
  height = c(2.5, 2.5),
  text = c("{photo_1}", "{photo_2}"),
  name = c("Photo marker 1", "Photo marker 2"),
  background = "#F0F0F0",
  line = sp_line(color = "#878C96", lwd = 1)
)
save_thumb(x, 1, "thumb-before.png")

# --- Thumbnail 2: After image_insert with fit = "inside" ---
x_inside <- image_insert(
  x,
  image = c(img_path, img_path),
  pattern = c("{photo_1}", "{photo_2}"),
  fit = "inside"
)
save_thumb(x_inside, 1, "thumb-after-inside.png")

# --- Thumbnail 3: fit = "fill" ---
x_fill <- image_insert(
  x,
  image = c(img_path, img_path),
  pattern = c("{photo_1}", "{photo_2}"),
  fit = "fill"
)
save_thumb(x_fill, 1, "thumb-after-fill.png")

# --- Thumbnail 4: After hiding markers ---
targets <- shape_select(x_inside, name = "Photo marker", match = "contains")
x_hidden <- shape_update(x_inside, targets, hidden = TRUE)
save_thumb(x_hidden, 1, "thumb-after-hidden.png")

message("Done. Thumbnails written to: ", out_dir)
