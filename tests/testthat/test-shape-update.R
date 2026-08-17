shape_update_emu <- function(x) {
  as.integer(round(x * 914400))
}


shape_update_add_xml <- function(x, xml) {
  slide <- x$slide$get_slide(x$cursor)$get()
  sp_tree <- xml2::xml_find_first(slide, "//p:spTree")
  node <- xml2::read_xml(xml)
  xml2::xml_add_child(sp_tree, node)
  x
}


shape_update_add_free_shape <- function(x, id = "2001", name = "Free marker",
                                        text = "{status}", left = 1, top = 1,
                                        width = 2, height = 1) {
  shape_update_add_xml(
    x,
    paste0(
      '<p:sp xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main" ',
      'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">',
      '<p:nvSpPr><p:cNvPr id="', id, '" name="', name, '"/>',
      "<p:cNvSpPr/><p:nvPr/></p:nvSpPr>",
      '<p:spPr><a:xfrm><a:off x="', shape_update_emu(left), '" y="', shape_update_emu(top), '"/>',
      '<a:ext cx="', shape_update_emu(width), '" cy="', shape_update_emu(height), '"/>',
      '</a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom></p:spPr>',
      '<p:txBody><a:bodyPr/><a:lstStyle/><a:p><a:r><a:rPr lang="en-US"/>',
      "<a:t>", text, "</a:t></a:r></a:p></p:txBody>",
      "</p:sp>"
    )
  )
}


shape_update_add_connector <- function(x, id = "2002", name = "Free connector",
                                       left = 4, top = 1, width = 2, height = 0) {
  shape_update_add_xml(
    x,
    paste0(
      '<p:cxnSp xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main" ',
      'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">',
      '<p:nvCxnSpPr><p:cNvPr id="', id, '" name="', name, '"/>',
      "<p:cNvCxnSpPr/><p:nvPr/></p:nvCxnSpPr>",
      '<p:spPr><a:xfrm><a:off x="', shape_update_emu(left), '" y="', shape_update_emu(top), '"/>',
      '<a:ext cx="', shape_update_emu(width), '" cy="', shape_update_emu(height), '"/>',
      '</a:xfrm><a:prstGeom prst="line"><a:avLst/></a:prstGeom></p:spPr>',
      "</p:cxnSp>"
    )
  )
}


shape_update_deck <- function() {
  x <- read_pptx()
  x <- add_slide(x, layout = "Blank")
  x <- shape_update_add_free_shape(x)
  x <- shape_update_add_connector(x)
  x <- ph_with(
    x,
    "{placeholder}",
    location = ph_location(left = 1, top = 2.5, width = 2, height = 1, newlabel = "Placeholder marker")
  )
  x
}


test_that("shape_update modifies ordinary non-placeholder shapes", {
  x <- shape_update_deck()
  sel <- shape_select(x, name = "Free marker", match = "exact")

  expect_false(sel$placeholder)

  x <- shape_update(
    x,
    sel,
    text = "Delayed",
    background = "#FCE8E6",
    line = sp_line(color = "#EC0016", lwd = 1.5),
    left = 1.5,
    top = 1.25,
    width = 3,
    height = 0.75,
    rotation = 15,
    name = "Status marker",
    description = "Current service status",
    hidden = TRUE
  )

  updated <- shape_select(x, name = "Status marker", match = "exact")
  node <- updated$node[[1]]

  expect_false(updated$placeholder)
  expect_equal(updated$text, "Delayed")
  expect_equal(updated$left, 1.5)
  expect_equal(updated$top, 1.25)
  expect_equal(updated$width, 3)
  expect_equal(updated$height, 0.75)
  expect_equal(updated$rotation, 15)
  expect_equal(updated$description, "Current service status")
  expect_true(updated$hidden)
  expect_length(xml2::xml_find_all(node, ".//p:nvPr/p:ph"), 0)
  expect_equal(xml2::xml_attr(xml2::xml_find_first(node, "p:spPr/a:solidFill/a:srgbClr"), "val"), "FCE8E6")
  expect_equal(xml2::xml_attr(xml2::xml_find_first(node, "p:spPr/a:ln/a:solidFill/a:srgbClr"), "val"), "EC0016")
})


test_that("shape_update supports vectorized values for duplicate selections", {
  x <- read_pptx()
  x <- add_slide(x, layout = "Blank")
  x <- shape_update_add_free_shape(x, id = "2001", name = "Marker 1", text = "{status}")
  x <- shape_update_add_free_shape(x, id = "2002", name = "Marker 2", text = "{status}", top = 2)

  sel <- shape_select(x, text = "{status}", match = "exact")
  x <- shape_update(x, sel, text = c("A", "B"), left = c(1, 2))
  updated <- shape_select(x, name = "Marker", match = "contains")

  expect_equal(updated$text, c("A", "B"))
  expect_equal(updated$left, c(1, 2))
})


test_that("shape_update handles placeholders as an additional case", {
  x <- shape_update_deck()
  sel <- shape_select(x, name = "Placeholder marker", match = "exact")

  expect_true(sel$placeholder)

  x <- shape_update(x, sel, text = "Updated placeholder", background = "#FFFFFF")
  updated <- shape_select(x, name = "Placeholder marker", match = "exact")

  expect_true(updated$placeholder)
  expect_equal(updated$text, "Updated placeholder")
  expect_equal(xml2::xml_attr(xml2::xml_find_first(updated$node[[1]], "p:spPr/a:solidFill/a:srgbClr"), "val"), "FFFFFF")
})


test_that("shape_update updates connector geometry, line, and metadata", {
  x <- shape_update_deck()
  sel <- shape_select(x, name = "Free connector", match = "exact")

  expect_false(sel$placeholder)

  x <- shape_update(
    x,
    sel,
    line = sp_line(color = "#000000", lwd = 2),
    left = 3,
    width = 4,
    name = "Updated connector",
    hidden = TRUE
  )
  updated <- shape_select(x, name = "Updated connector", match = "exact")

  expect_equal(updated$kind, "connector")
  expect_false(updated$placeholder)
  expect_equal(updated$left, 3)
  expect_equal(updated$width, 4)
  expect_true(updated$hidden)
  expect_length(xml2::xml_find_all(updated$node[[1]], ".//p:nvPr/p:ph"), 0)
  expect_equal(xml2::xml_attr(xml2::xml_find_first(updated$node[[1]], "p:spPr/a:ln/a:solidFill/a:srgbClr"), "val"), "000000")
})


test_that("shape_update validates unsupported and invalid updates before changes", {
  x <- read_pptx()
  x <- add_slide(x, layout = "Blank")
  x <- ph_with(
    x,
    external_img(test_image("dog_1")),
    location = ph_location(left = 1, top = 1, width = 1, height = 1, newlabel = "Picture marker")
  )
  sel <- shape_select(x, name = "Picture marker", match = "exact")
  old_name <- sel$name

  expect_error(shape_update(x, sel, background = "#FFFFFF"), "not supported")
  expect_error(shape_update(x, sel, text = "Nope"), "not supported")
  expect_equal(shape_select(x, name = old_name, match = "exact")$name, old_name)

  text_x <- shape_update_deck()
  text_sel <- shape_select(text_x, name = "Free marker", match = "exact")
  expect_error(shape_update(shape_update_deck(), text_sel, text = "Nope"), "different presentation")
  expect_error(shape_update(text_x, text_sel[0, ], text = "Nope"), "empty")
  expect_s3_class(shape_update(text_x, text_sel[0, ], text = "Nope", empty = "ignore"), "rpptx")
  expect_error(shape_update(text_x, text_sel, text = c("A", "B")), "length 1")
})


test_that("shape_update rejects invalid transform values before XML mutation", {
  x <- shape_update_deck()
  sel <- shape_select(x, name = "Free marker", match = "exact")
  node <- sel$node[[1]]
  off <- xml2::xml_find_first(node, "p:spPr/a:xfrm/a:off")
  xfrm <- xml2::xml_find_first(node, "p:spPr/a:xfrm")
  old_x <- xml2::xml_attr(off, "x")

  expect_error(shape_update(x, sel, left = Inf), "finite")
  expect_error(shape_update(x, sel, top = -Inf), "finite")
  expect_error(shape_update(x, sel, width = Inf), "finite")
  expect_error(shape_update(x, sel, rotation = Inf), "finite")
  expect_error(
    shape_update(x, sel, left = .Machine$integer.max / 914400 + 1),
    "supported range"
  )
  expect_error(
    shape_update(x, sel, rotation = .Machine$integer.max / 60000 + 1),
    "supported range"
  )

  expect_equal(xml2::xml_attr(off, "x"), old_x)
  expect_false(identical(xml2::xml_attr(off, "x"), "NA"))
  expect_false(identical(xml2::xml_attr(xfrm, "rot"), "NA"))
})


test_that("shape_update detects stale selections", {
  x <- shape_update_deck()
  sel <- shape_select(x, name = "Free marker", match = "exact")

  x <- add_slide(x, layout = "Blank")
  x <- move_slide(x, index = 2, to = 1)
  expect_error(shape_update(x, sel, text = "Nope"), "stale")

  x <- shape_update_deck()
  sel <- shape_select(x, name = "Free marker", match = "exact")
  xml2::xml_remove(sel$node[[1]])
  expect_error(shape_update(x, sel, text = "Nope"), "stale")

  x <- shape_update_deck()
  sel <- shape_select(x, name = "Free marker", match = "exact")
  print(x, target = tempfile(fileext = ".pptx"))
  expect_error(shape_update(x, sel, text = "Nope"), "stale")
})


test_that("shape_update changes survive write and reload", {
  x <- shape_update_deck()
  sel <- shape_select(x, name = "Free marker", match = "exact")
  x <- shape_update(
    x,
    sel,
    text = "Reloaded",
    background = "transparent",
    description = NA_character_,
    hidden = FALSE
  )
  file <- tempfile(fileext = ".pptx")
  print(x, target = file)

  y <- read_pptx(file)
  updated <- shape_select(y, name = "Free marker", match = "exact")

  expect_equal(updated$text, "Reloaded")
  expect_false(updated$placeholder)
  expect_false(updated$hidden)
  expect_true(is.na(updated$description))
  expect_length(xml2::xml_find_all(updated$node[[1]], ".//p:nvPr/p:ph"), 0)
  expect_length(xml2::xml_find_all(updated$node[[1]], "p:spPr/a:noFill"), 1)
})
