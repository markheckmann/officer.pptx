image_insert_deck <- function() {
  x <- read_pptx()
  x <- add_slide(x, layout = "Blank")
  shape_add(
    x,
    left = 1,
    top = 1,
    width = 2,
    height = 1,
    text = "{image_1}",
    name = "Image marker"
  )
}


test_that("image_insert inserts a non-placeholder picture over pattern targets", {
  x <- image_insert_deck()

  x <- image_insert(
    x,
    image = test_image("flag_de"),
    pattern = "{image_1}",
    fit = "fill"
  )

  marker <- shape_select(x, name = "Image marker", match = "exact")
  picture <- shape_select(x, kind = "picture")

  expect_equal(nrow(marker), 1)
  expect_equal(nrow(picture), 1)
  expect_false(picture$placeholder)
  expect_equal(picture$name, basename(test_image("flag_de")))
  expect_equal(picture$left, marker$left)
  expect_equal(picture$top, marker$top)
  expect_equal(picture$width, marker$width)
  expect_equal(picture$height, marker$height)
  expect_gt(picture$shape_idx, marker$shape_idx)
})


test_that("image_insert accepts selections and preserves the cursor", {
  x <- image_insert_deck()
  x <- add_slide(x, layout = "Blank")
  x <- on_slide(x, 2)
  old_cursor <- x$cursor

  x <- shape_select(x, slide_idx = 1, text = "{image_1}", match = "exact") |>
    image_insert(image = test_image("flag_de"), fit = "fill")

  expect_identical(x$cursor, old_cursor)
  expect_equal(nrow(shape_select(x, slide_idx = 1, kind = "picture")), 1)
  expect_equal(nrow(shape_select(x, slide_idx = 2, kind = "picture")), 0)
})


test_that("image_insert follows visible slide order", {
  x <- read_pptx()
  x <- add_slide(x, layout = "Blank")
  x <- shape_add(x, left = 1, top = 1, width = 1, height = 1, text = "{first}", name = "First")
  x <- add_slide(x, layout = "Blank")
  x <- shape_add(x, left = 1, top = 1, width = 1, height = 1, text = "{second}", name = "Second")
  x <- move_slide(x, index = 2, to = 1)

  x <- image_insert(
    x,
    image = test_image("flag_de"),
    pattern = "{second}",
    slide_idx = 1,
    fit = "fill"
  )

  expect_equal(nrow(shape_select(x, slide_idx = 1, kind = "picture")), 1)
  expect_equal(nrow(shape_select(x, slide_idx = 2, kind = "picture")), 0)
})


test_that("image_insert survives write and reload", {
  x <- image_insert_deck()
  x <- image_insert(x, test_image("flag_de"), pattern = "{image_1}", fit = "fill")
  file <- tempfile(fileext = ".pptx")
  print(x, target = file)

  y <- read_pptx(file)
  picture <- shape_select(y, kind = "picture")

  expect_equal(nrow(picture), 1)
  expect_false(picture$placeholder)
  expect_equal(picture$name, basename(test_image("flag_de")))
})


test_that("image_insert validates before modifying the deck", {
  x <- image_insert_deck()
  before <- nrow(shape_select(x))

  expect_error(
    image_insert(x, image = test_image("flag_de"), pattern = "{missing}"),
    "No shape matched"
  )
  expect_error(
    image_insert(x, image = c(test_image("flag_de"), test_image("dog_1")), pattern = c("a", "b", "c")),
    "common length"
  )
  expect_error(
    image_insert(x, image = "does-not-exist.png", pattern = "{image_1}"),
    "does-not-exist|No such|cannot open|unable"
  )
  expect_error(
    image_insert(x, image = test_image("flag_de"), pattern = "{image_1}", scale = -1),
    "scale"
  )

  expect_equal(nrow(shape_select(x)), before)
})


test_that("image_insert can ignore empty selections", {
  x <- image_insert_deck()

  y <- image_insert(
    x,
    image = test_image("flag_de"),
    pattern = "{missing}",
    empty = "ignore"
  )

  expect_equal(nrow(shape_select(y, kind = "picture")), 0)
})
