x <- read_pptx() |>
  add_slide(
    layout = "Title Slide",
    ctrTitle = "Hello World",
    subTitle = "Here we go",
    ftr = "Ths is a footer!",
    dt = as.character(Sys.Date()),
    sldNum = 99
  )
slide_preview(x)

# show existing presentation slide
file <- example_pptx("text_replace")
x <- read_pptx(file)
slide_preview(x)
