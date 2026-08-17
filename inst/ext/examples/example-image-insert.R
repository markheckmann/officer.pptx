file_in <- example_pptx("image_insert")
file_out <- tempfile(fileext = ".pptx")

x <- read_pptx(file_in)

x <- image_insert(
  x,
  image = example_image("dog_1"),
  pattern = "{2}",
  fit = "inside"
)

targets <- shape_select(x, text = "{3}", match = "exact")
x <- image_insert(targets, image = example_image("dog_2"), fit = "fill")

print(x, target = file_out)
