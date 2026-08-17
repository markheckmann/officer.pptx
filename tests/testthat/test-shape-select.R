shape_select_deck <- function() {
  x <- read_pptx()
  x <- add_slide(x, layout = "Blank")
  x <- ph_with(
    x,
    "{image_1}",
    location = ph_location(
      left = 1, top = 1, width = 2, height = 1,
      newlabel = "Image marker 1"
    )
  )
  x <- ph_with(
    x,
    "  {image_1}\n",
    location = ph_location(
      left = 3.5, top = 1, width = 2, height = 1,
      newlabel = "Image marker 1 duplicate"
    )
  )
  x <- ph_with(
    x,
    "Text before {image_1}",
    location = ph_location(
      left = 1, top = 2.5, width = 2, height = 1,
      newlabel = "Contains marker"
    )
  )
  x <- ph_with(
    x,
    external_img(test_image("dog_1")),
    location = ph_location(
      left = 5.8, top = 1, width = 1, height = 1,
      newlabel = "Picture marker"
    )
  )
  x <- shape_select_add_group(x)

  x <- add_slide(x, layout = "Blank")
  x <- ph_with(
    x,
    "{image_1}",
    location = ph_location(
      left = 1, top = 1, width = 2, height = 1,
      newlabel = "Slide 2 marker"
    )
  )
  x
}


shape_select_add_group <- function(x) {
  slide <- x$slide$get_slide(x$cursor)$get()
  sp_tree <- xml2::xml_find_first(slide, "//p:spTree")
  group <- xml2::read_xml(
    paste0(
      '<p:grpSp xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main" ',
      'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">',
      "<p:nvGrpSpPr>",
      '<p:cNvPr id="1000" name="Grouped markers"/>',
      "<p:cNvGrpSpPr/>",
      "<p:nvPr/>",
      "</p:nvGrpSpPr>",
      "<p:grpSpPr>",
      "<a:xfrm>",
      '<a:off x="6400800" y="914400"/>',
      '<a:ext cx="914400" cy="914400"/>',
      '<a:chOff x="0" y="0"/>',
      '<a:chExt cx="914400" cy="914400"/>',
      "</a:xfrm>",
      "</p:grpSpPr>",
      "<p:sp>",
      "<p:nvSpPr>",
      '<p:cNvPr id="1001" name="Grouped child"/>',
      "<p:cNvSpPr/>",
      "<p:nvPr/>",
      "</p:nvSpPr>",
      "<p:spPr>",
      "<a:xfrm>",
      '<a:off x="0" y="0"/>',
      '<a:ext cx="914400" cy="914400"/>',
      "</a:xfrm>",
      "</p:spPr>",
      "<p:txBody>",
      "<a:bodyPr/>",
      "<a:lstStyle/>",
      "<a:p><a:r><a:t>{group_child}</a:t></a:r></a:p>",
      "</p:txBody>",
      "</p:sp>",
      "</p:grpSp>"
    )
  )
  xml2::xml_add_child(sp_tree, group)
  x
}


test_that("shape_select returns top-level shapes and metadata", {
  x <- shape_select_deck()

  sel <- shape_select(x, slide_idx = 1)

  expect_s3_class(sel, "pptx_shape_selection")
  expect_named(sel, c(
    "slide_idx", "shape_idx", "id", "name", "text", "kind",
    "placeholder", "ph_type", "hidden", "left", "top", "width",
    "height", "rotation", "description", "node"
  ))
  expect_true(all(sel$slide_idx == 1L))
  expect_true(all(diff(sel$shape_idx) > 0L))
  expect_true("shape" %in% sel$kind)
  expect_true("picture" %in% sel$kind)
  expect_true("group" %in% sel$kind)
  expect_true(inherits(sel$node[[1]], "xml_node"))
  expect_equal(sel$left[sel$name == "Image marker 1"], 1)
  expect_equal(sel$width[sel$name == "Image marker 1"], 2)
})


test_that("shape_select matches visible text with contains by default", {
  x <- shape_select_deck()

  sel <- shape_select(x, text = "{image_1}")

  expect_equal(nrow(sel), 4)
  expect_equal(sel$name, c(
    "Image marker 1",
    "Image marker 1 duplicate",
    "Contains marker",
    "Slide 2 marker"
  ))
})


test_that("shape_select exact matching trims surrounding whitespace", {
  x <- shape_select_deck()

  sel <- shape_select(x, slide_idx = 1, text = "{image_1}", match = "exact")

  expect_equal(sel$name, c("Image marker 1", "Image marker 1 duplicate"))
})


test_that("shape_select supports regex and combined filters", {
  x <- shape_select_deck()

  sel <- shape_select(
    x,
    slide_idx = 1,
    text = "^\\s*\\{image_[0-9]+\\}\\s*$",
    name = "duplicate",
    match = "regex"
  )

  expect_equal(nrow(sel), 1)
  expect_equal(sel$name, "Image marker 1 duplicate")
})


test_that("shape_select filters by metadata", {
  x <- shape_select_deck()

  expect_equal(shape_select(x, kind = "picture")$name, "Picture marker")
  expect_equal(shape_select(x, id = "1000")$name, "Grouped markers")
  expect_equal(shape_select(x, hidden = TRUE) |> nrow(), 0)
  expect_true(all(shape_select(x, placeholder = FALSE)$placeholder == FALSE))
})


test_that("shape_select treats groups as top-level objects only", {
  x <- shape_select_deck()

  group <- shape_select(x, kind = "group")
  child <- shape_select(x, text = "{group_child}")

  expect_equal(group$name, "Grouped markers")
  expect_equal(group$text, NA_character_)
  expect_equal(nrow(child), 0)
})


test_that("shape_select follows visible slide order", {
  x <- read_pptx()
  x <- add_slide(x, layout = "Blank")
  x <- ph_with(x, "{slide_1}", location = ph_location(newlabel = "First"))
  x <- add_slide(x, layout = "Blank")
  x <- ph_with(x, "{slide_2}", location = ph_location(newlabel = "Second"))
  x <- move_slide(x, index = 2, to = 1)

  sel <- shape_select(x, text = "{slide_", match = "contains")

  expect_equal(sel$slide_idx, c(1L, 2L))
  expect_equal(sel$text, c("{slide_2}", "{slide_1}"))
})


test_that("shape_select does not change the cursor", {
  x <- shape_select_deck()
  x <- on_slide(x, 2)
  old_cursor <- x$cursor

  shape_select(x, slide_idx = 1, text = "{image_1}")

  expect_identical(x$cursor, old_cursor)
})


test_that("shape_select validates inputs", {
  x <- shape_select_deck()

  expect_error(shape_select(x, slide_idx = 99), "invalid indices")
  expect_error(shape_select(x, hidden = NA), "logical")
  expect_error(shape_select(x, text = NA_character_), "without missing")
})
