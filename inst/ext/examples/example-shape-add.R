x <- read_pptx()
x <- add_slide(x, layout = "Blank")

x <- shape_add(
  x,
  left = c(0.8, 3.15, 5.5),
  top = 1.45,
  width = 1.9,
  height = 1.05,
  text = c("On time", "Delayed", "Cancelled"),
  geometry = "roundRect",
  background = c("#E6F4EA", "#FFF4CE", "#FCE8E6"),
  line = list(
    sp_line(color = "#408335", lwd = 1.5),
    sp_line(color = "#F39200", lwd = 1.5),
    sp_line(color = "#EC0016", lwd = 1.5)
  ),
  name = c("Status 1", "Status 2", "Status 3")
)

shape_select(x, name = "Status", match = "contains")
