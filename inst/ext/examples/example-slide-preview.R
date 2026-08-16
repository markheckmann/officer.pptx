x <- read_pptx()
x <- add_slide(x,
  layout = "Title Slide",
  ctrTitle = "Hello World",
  subTitle = "Here we go",
  ftr = "Ths is a footer!",
  dt = as.character(Sys.Date()),
  sldNum = 1
)
if (nzchar(Sys.which("soffice"))) {
  slide_preview(x)
}
