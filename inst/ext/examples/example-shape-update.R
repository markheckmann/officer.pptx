x <- read_pptx()
x <- add_slide(x, layout = "Blank")
x <- ph_with(
  x,
  "{status}",
  location = ph_location(
    left = 1, top = 1, width = 2.5, height = 1,
    newlabel = "Status marker"
  )
)

targets <- shape_select(x, text = "{status}", match = "exact")

x <- shape_update(
  x,
  targets,
  text = "Delayed",
  background = "#FCE8E6",
  line = sp_line(color = "#EC0016", lwd = 1.5),
  description = "Current service status"
)

shape_select(x, name = "Status marker", match = "exact")
