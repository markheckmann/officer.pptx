# test_that("text replace work without erros", {
#   run_example <- \() {
#     file_in <- test_file("testdata_01_replace")
#     file_out <- tempfile(fileext = ".pptx")
#
#     x <- read_pptx(file_in)
#     x <- pptx_text_replace(x, "@" = "<<>>", "{1}" = "ö")
#     print(x, target = file_out)
#   }
#
#   pptx_image_insert_at_shape_temp
#
#   expect_no_error(run_example())
# })


test_that("inserting sample image at shape position works", {
  run_example <- \() {
    x <- example_file("image_insert") |> read_pptx()
    img_path <- test_image("dog_1")
    df <- pptx_shapes_info(x)
    node <- df$node[[2]]
    xml_add_image_at_shape(x, img_path, shape = node)
  }

  expect_no_error(run_example())

  xml_content <- "<root><child>Value</child></root>"
  xml_doc <- read_xml(xml_content)
  node <- xml_find_first(xml_doc, "//child")


  node <- xml2:::xml_nodeset()
  class(node)
  .foo <- \(x, y) list(x = x, y = y)
  foo <- Vectorize(.foo, vectorize.args = c("x", "y"), SIMPLIFY = FALSE)
  foo(list(node), 1)


  mapply(.foo, x = node)
  x <- foo(node)
  class(x)
})
