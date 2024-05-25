test_that("xml and pptx helper functions work", {
  file_in <- test_file("testdata_02_xml")
  x <- read_pptx(file_in)

  # finds all shapes and onyl shapes
  shapes <- pptx_shapes_on_slide(x, 1)
  expect_equal(length(shapes), 4)
  expect_true(xml_is_shape(shapes), info = "extracts shapes only")

  # hide and unhide shapes
  expect_true(xml_shape_attr_hidden(shapes) |> is.na() |> all())
  xml_shape_hide(shapes)
  expect_true((xml_shape_attr_hidden(shapes) == "1") |> all())
  xml_shape_unhide(shapes)
  expect_true((xml_shape_attr_hidden(shapes) == "0") |> all())

  # remove shape
  xml_shape_remove(shapes[1:2])
  shapes_new <- pptx_shapes_on_slide(x, 1)
  expect_equal(length(shapes_new), 2)
})
