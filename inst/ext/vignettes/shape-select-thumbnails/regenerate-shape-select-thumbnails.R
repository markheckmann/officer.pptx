#!/usr/bin/env Rscript

# Regenerate thumbnails used by vignettes/shape-select.qmd.
# Run from the package root.
# Requires LibreOffice, pdftools, png, magick, and the local package sources.

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
} else {
  stop("Package 'devtools' is required to load the local package sources.", call. = FALSE)
}

if (!requireNamespace("magick", quietly = TRUE)) {
  stop("Package 'magick' is required to add thumbnail borders.", call. = FALSE)
}

shape_example_pptx <- function() {
  path <- "inst/ext/pptx/shapes.pptx"
  if (!file.exists(path)) {
    stop("Run this script from the package root. Missing: ", path, call. = FALSE)
  }
  path
}

out_dir <- "inst/ext/vignettes/shape-select-thumbnails"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

border_color <- "#878C96"

save_thumb <- function(deck, slide_idx, name) {
  tmp <- slide_to_png(deck, slide_idx = slide_idx)
  img <- magick::image_read(tmp)
  img <- magick::image_border(img, border_color, "3x3")
  out <- file.path(out_dir, name)
  magick::image_write(img, out)
  message("Wrote ", out)
  invisible(out)
}

x <- read_pptx(shape_example_pptx())
save_thumb(x, 1, "thumb-shape-marker.png")

x <- read_pptx(shape_example_pptx())
x <- x |>
  shape_select(text = "{tag_1}", match = "exact") |>
  shape_update(
    text = "Image placeholder",
    background = "#E6F4EA",
    line = sp_line(color = "#408335", lwd = 1.5),
    name = "Image placeholder marker"
  )
stopifnot(identical(
  shape_select(x, name = "Image placeholder marker", match = "exact")$text,
  "Image placeholder"
))
save_thumb(x, 1, "thumb-shape-typical-update.png")

x <- shape_add(
  x,
  left = 0.826,
  top = 2.25,
  width = 8.19,
  height = 0.8,
  text = "New shape",
  geometry = "roundRect",
  background = "#E6F4EA",
  line = sp_line(color = "#408335", lwd = 1.5),
  name = "Added shape"
)
save_thumb(x, 1, "thumb-shape-add.png")

save_thumb(read_pptx(shape_example_pptx()), 1, "thumb-shape-update-before.png")

template <- read_pptx(shape_example_pptx())
targets <- shape_select(template, text = "{tag_", match = "contains")
status <- c(
  "{tag_1}" = "On time",
  "{tag_2}" = "Delayed",
  "{tag_3}" = "Cancelled"
)
background <- c(
  "{tag_1}" = "#E6F4EA",
  "{tag_2}" = "#FFF4CE",
  "{tag_3}" = "#FCE8E6"
)
line <- list(
  "{tag_1}" = sp_line(color = "#408335", lwd = 1.5),
  "{tag_2}" = sp_line(color = "#F39200", lwd = 1.5),
  "{tag_3}" = sp_line(color = "#EC0016", lwd = 1.5)
)
description <- c(
  "{tag_1}" = "Green status marker",
  "{tag_2}" = "Yellow status marker",
  "{tag_3}" = "Red status marker"
)
updated <- shape_update(
  template,
  targets,
  text = unname(status[targets$text]),
  background = unname(background[targets$text]),
  line = unname(line[targets$text]),
  description = unname(description[targets$text]),
  name = paste("Status marker", seq_len(nrow(targets)))
)
stopifnot(identical(
  shape_select(updated, name = "Status marker", match = "contains")$text,
  unname(status[targets$text])
))
save_thumb(updated, 1, "thumb-shape-update-after.png")
