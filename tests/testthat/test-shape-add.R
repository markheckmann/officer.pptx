shape_add_count <- function(x, slide_idx = 1) {
  nrow(shape_select(x, slide_idx = slide_idx))
}


shape_add_sp_tree <- function(x, slide_idx = 1) {
  slide <- pptx_slide_get_visible(x, slide_idx)
  xml_slide_shape_tree(slide)
}


test_that("shape_add creates ordinary non-placeholder shapes", {
  x <- read_pptx()
  x <- add_slide(x, layout = "Blank")

  x <- shape_add(
    x,
    left = 1,
    top = 1.2,
    width = 2.5,
    height = 1,
    text = "Status",
    geometry = "roundRect",
    background = "#E6F4EA",
    line = sp_line(color = "#408335", lwd = 1.5),
    rotation = 10,
    name = "Status shape",
    description = "Status description",
    hidden = TRUE
  )

  added <- shape_select(x, name = "Status shape", match = "exact")
  node <- added$node[[1]]

  expect_equal(nrow(added), 1)
  expect_equal(added$kind, "shape")
  expect_false(added$placeholder)
  expect_equal(added$text, "Status")
  expect_equal(added$left, 1)
  expect_equal(added$top, 1.2)
  expect_equal(added$width, 2.5)
  expect_equal(added$height, 1)
  expect_equal(added$rotation, 10)
  expect_equal(added$description, "Status description")
  expect_true(added$hidden)
  expect_equal(xml2::xml_attr(xml2::xml_find_first(node, "p:spPr/a:prstGeom"), "prst"), "roundRect")
  expect_equal(xml2::xml_attr(xml2::xml_find_first(node, "p:spPr/a:solidFill/a:srgbClr"), "val"), "E6F4EA")
  expect_equal(xml2::xml_attr(xml2::xml_find_first(node, "p:spPr/a:ln/a:solidFill/a:srgbClr"), "val"), "408335")
  expect_length(xml2::xml_find_all(node, ".//p:nvPr/p:ph"), 0)
})


test_that("shape_add vectorizes row-wise and generated names are selectable", {
  x <- read_pptx()
  x <- add_slide(x, layout = "Blank")

  x <- shape_add(
    x,
    left = c(0.8, 3.15, 5.5),
    top = 1.45,
    width = 1.9,
    height = 1.05,
    text = c("On time", "Delayed", "Cancelled"),
    background = c("#E6F4EA", "#FFF4CE", "#FCE8E6"),
    line = list(
      sp_line(color = "#408335", lwd = 1.5),
      sp_line(color = "#F39200", lwd = 1.5),
      sp_line(color = "#EC0016", lwd = 1.5)
    )
  )

  added <- shape_select(x, name = "Shape", match = "contains")

  expect_equal(nrow(added), 3)
  expect_equal(added$name, c("Shape 1", "Shape 2", "Shape 3"))
  expect_equal(added$text, c("On time", "Delayed", "Cancelled"))
  expect_equal(added$left, c(0.8, 3.15, 5.5))
  expect_equal(added$top, rep(1.45, 3))
  expect_false(any(added$placeholder))
})


test_that("shape_add integrates with shape_update", {
  x <- read_pptx()
  x <- add_slide(x, layout = "Blank")
  x <- shape_add(x, left = 1, top = 1, width = 2, height = 1, text = "Old", name = "Added")

  sel <- shape_select(x, name = "Added", match = "exact")
  x <- shape_update(x, sel, text = "New", background = "#FCE8E6")
  updated <- shape_select(x, name = "Added", match = "exact")

  expect_equal(updated$text, "New")
  expect_equal(xml2::xml_attr(xml2::xml_find_first(updated$node[[1]], "p:spPr/a:solidFill/a:srgbClr"), "val"), "FCE8E6")
  expect_false(updated$placeholder)
})


test_that("shape_add follows visible slide order and preserves cursor", {
  x <- read_pptx()
  x <- add_slide(x, layout = "Blank")
  x <- shape_add(x, left = 1, top = 1, width = 1, height = 1, text = "First", name = "First")
  x <- add_slide(x, layout = "Blank")
  x <- shape_add(x, left = 1, top = 1, width = 1, height = 1, text = "Second", name = "Second")
  x <- move_slide(x, index = 2, to = 1)
  x <- on_slide(x, 2)
  old_cursor <- x$cursor

  x <- shape_add(x, slide_idx = 1, left = 2, top = 1, width = 1, height = 1, text = "Visible first", name = "Visible first")

  expect_identical(x$cursor, old_cursor)
  expect_equal(shape_select(x, slide_idx = 1, name = "Visible first", match = "exact")$text, "Visible first")
  expect_equal(nrow(shape_select(x, slide_idx = 2, name = "Visible first", match = "exact")), 0)
})


test_that("shape_add appends in input order and before extLst", {
  x <- read_pptx()
  x <- add_slide(x, layout = "Blank")
  x <- shape_add(x, left = 1, top = 1, width = 1, height = 1, text = "Existing", name = "Existing")
  before <- shape_select(x, slide_idx = 1, name = "Existing", match = "exact")

  x <- shape_add(
    x,
    left = c(2, 3), top = 1, width = 1, height = 1,
    text = c("A", "B"), name = c("Added A", "Added B")
  )
  after <- shape_select(x, slide_idx = 1, name = c("Existing", "Added"), match = "contains")

  expect_equal(after$name, c("Existing", "Added A", "Added B"))
  expect_equal(after$shape_idx[after$name == "Existing"], before$shape_idx)

  sp_tree <- shape_add_sp_tree(x, slide_idx = 1)
  ext_lst <- xml2::read_xml(
    '<p:extLst xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"/>'
  )
  xml2::xml_add_child(sp_tree, ext_lst)
  x <- shape_add(x, left = 4, top = 1, width = 1, height = 1, text = "C", name = "Before extLst")

  names <- xml2::xml_name(xml2::xml_children(sp_tree))
  expect_equal(tail(names, 2), c("sp", "extLst"))
})


test_that("shape_add escapes special XML characters and survives reload", {
  x <- read_pptx()
  x <- add_slide(x, layout = "Blank")
  x <- shape_add(
    x,
    left = 1,
    top = 1,
    width = 2,
    height = 1,
    text = "A < B & C",
    name = "Name <&>",
    description = "Description <&>",
    background = "transparent",
    hidden = FALSE
  )
  file <- tempfile(fileext = ".pptx")
  print(x, target = file)

  y <- read_pptx(file)
  added <- shape_select(y, name = "Name <&>", match = "exact")

  expect_equal(added$text, "A < B & C")
  expect_equal(added$description, "Description <&>")
  expect_false(added$placeholder)
  expect_false(added$hidden)
  expect_length(xml2::xml_find_all(added$node[[1]], "p:spPr/a:noFill"), 1)
  expect_length(unique(shape_select(y, slide_idx = 1)$id), nrow(shape_select(y, slide_idx = 1)))
})


test_that("shape_add validates inputs before inserting shapes", {
  x <- read_pptx()
  x <- add_slide(x, layout = "Blank")
  before <- shape_add_count(x)

  expect_error(shape_add(x, left = 1, top = 1, width = 0, height = 1), "positive")
  expect_error(shape_add(x, left = Inf, top = 1, width = 1, height = 1), "finite")
  expect_error(
    shape_add(x, left = .Machine$integer.max / 914400 + 1, top = 1, width = 1, height = 1),
    "supported range"
  )
  expect_error(
    shape_add(x, left = 1, top = 1, width = 1, height = 1, rotation = .Machine$integer.max / 60000 + 1),
    "supported range"
  )
  expect_error(shape_add(x, left = 1, top = 1, width = 1, height = 1, geometry = "not-a-shape"), "valid geometry")
  expect_error(shape_add(x, left = 1:2, top = 1:3, width = 1, height = 1), "length 1")
  expect_error(shape_add(x, left = 1, top = 1, width = 1, height = 1, slide_idx = 99), "invalid indices")
  expect_error(shape_add(x, left = 1, top = 1, width = 1, height = 1, background = "not-a-color"), "invalid color")
  expect_error(shape_add(x, left = 1, top = 1, width = 1, height = 1, text = NA_character_), "without missing")
  expect_error(shape_add(read_pptx(), left = 1, top = 1, width = 1, height = 1), "no slides")

  expect_equal(shape_add_count(x), before)
})
