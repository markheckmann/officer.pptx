library(officer)

devtools::load_all()

file_in <- example_file("image_insert")
file_out <- "dev_local/02_insert_image/image_insert_FILLED.pptx"
image <- "dev_local/02_insert_image/dog.jpeg"

pptx_shapes_info(x) |> tidyr::unnest_wider(info) #, info)





image <- c("dev_local/02_insert_image/dog.jpeg", "dev_local/02_insert_image/dog_2.png")
x <- read_pptx(file_in)
shapes <- x |> pptx_shapes_on_slide(1, pattern = "{1}")
x <- xml_add_image_at_shape(x, image, shapes, scale = 1, h_just = .5, v_just = .5)
print(x, target = file_out)
file_open(file_out)
file_open(file_in)
# both input version should be possible
img_add <- function(path, pattern = NULL, scale = 1, h_just = .5) {
  l <- list(path = path, pattern = pattern, scale = scale, h_just = h_just)
  class(l) <- c("img_add", "list")
  l
}
print.img_add <- function(x, ...) {
  str(x, 1, give.attr = FALSE, )
}
l <- list(
  img_add(path = "img/path", pattern = "{1}", scale = 1, h_just = .5),
  img_add(path = "img/path")
)
l
pattern <- c("{1}", "{2}")
image <- c("img/path_1", "img/path_2")

pptx_image_insert_at_shape <- function(x, image, pattern, ...) {


  xml_add_image_at_shape <- function(x, img_path, shape, ...) {
    .xml_add_image_at_shape_vec(x, img_path, shape, ...)
    x
  }

}

# tmp

library(officer.pptx)
file_in <- example_file("image_insert")

# file_open(file_in)
file_out <- "dev_local/02_insert_image/image_insert_FILLED.pptx"
# image <- "dev_local/02_insert_image/dog.jpeg"

image <- c("dev_local/02_insert_image/dog.jpeg", "dev_local/02_insert_image/dog_2.png")
image = rep(image, 2)
i=1:4
pattern <-  glue::glue("{{{i}}}") |> as.character()
x <- read_pptx(file_in)
x <- pptx_image_insert_at_shape_temp(x, image, pattern, slide_idx = 1:2)
print(x, target = file_out)
file_open(file_out)



# test

library(officer.pptx)
file_in <- example_file("image_insert")
img_path <- sapply(test_image(), test_image)
n <- length(img_path)
pattern <- glue::glue("{{{1:n}}}") |> as.character()
x <- read_pptx(file_in)
x <- pptx_image_insert_at_shape_temp(x, img_path, pattern)
print(x, target = file_out)
file_open(file_out)


x <- example_file("image_insert") |> read_pptx()
img_path <- test_image("dog_1")

df <- pptx_shapes_info(x)
node <- df$node[[2]]
.xml_add_image_at_shape(x, img_path, shape = node)

