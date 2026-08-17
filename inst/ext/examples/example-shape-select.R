x <- read_pptx()
x <- add_slide(x, layout = "Blank")
x <- ph_with(
  x,
  "{image_1}",
  location = ph_location(
    left = 1, top = 1, width = 2, height = 1,
    newlabel = "Image marker"
  )
)

# Select every top-level shape whose visible text contains the marker.
shape_select(x, text = "{image_1}")

# Use exact matching when the full marker text should match. Leading and
# trailing whitespace in the shape text is ignored.
shape_select(x, text = "{image_1}", match = "exact")
