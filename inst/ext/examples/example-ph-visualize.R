library(officer)
library(officer.pptx)

# All placeholders with default dashed border
read_pptx() |>
  add_slide(layout = "Two Content") |>
  ph_visualize()

# Specific placeholder with background color
read_pptx() |>
  add_slide(layout = "Title and Content") |>
  ph_visualize(ph = "body", background = "#FFE4E1")

# Custom border: red, thick, solid line
read_pptx() |>
  add_slide(layout = "Two Content") |>
  ph_visualize(ph = "title", line = sp_line(color = "red", lwd = 3, lty = "solid"))

# Multiple placeholders with labels
read_pptx() |>
  add_slide(layout = "Two Content") |>
  ph_visualize(ph = c("body[1]", "body[2]"), label = TRUE)

# Using ph_location_type()
read_pptx() |>
  add_slide(layout = "Title and Content") |>
  ph_visualize(ph = ph_location_type("body"), background = "lightblue")

# Combine with actual content
read_pptx() |>
  add_slide(layout = "Two Content") |>
  ph_visualize(ph = c("body[1]", "body[2]"), background = "#E3F2FD") |>
  ph_with("Left content", location = "body[1]") |>
  ph_with("Right content", location = "body[2]")

# Order matters: visualize before vs after adding content
# Left: image first, then visualization (overlay on top of image)
# Right: visualization first, then image (image covers overlay)
img_path <- file.path(R.home("doc"), "html", "logo.jpg")
read_pptx() |>
  add_slide(layout = "Two Content") |>
  ph_with(external_img(img_path), location = "body[1]") |>
  ph_visualize(ph = "body[1]", background = "#1565C080") |>
  ph_visualize(ph = "body[2]", background = "#1565C080") |>
  ph_with(external_img(img_path), location = "body[2]")
