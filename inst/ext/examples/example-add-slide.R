x <- read_pptx()
layout_summary(x)
x <- add_slide(x,
  layout = "Two Content", master = "Office Theme"
)

# use ... to fill slide placeholder in same step
x <- read_pptx() |>
  add_slide("Two Content",
            `Title 1` = "A title",
            dt = "Jan. 26, 2025",
            `body[2]` = "Body 2",
            left = "Left side",
            `6` = "Footer")
