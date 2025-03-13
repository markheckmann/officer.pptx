get_shapetree <- function(x, slide_idx = NULL) {
  officer:::stop_if_not_rpptx(x)
  slide_idx <- slide_idx %||% x$cursor
  xml_node <- x$slide$get_slide(slide_idx)$get()
  xml2::xml_child(xml_node, "*/p:spTree")
}


get_shapetrees <- function(x, slide_idx = NULL) {
  officer:::stop_if_not_rpptx(x)
  slide_idx <- slide_idx %||% seq_len(length(x))
  lapply(slide_idx, function(idx) get_shapetree(x, idx))
}


# all slide's shapetrees as a string and shape's UUIDs removed
get_shapetrees_string <- function(x, slide_idx = NULL) {
  officer:::stop_if_not_rpptx(x)
  sp_tree <- get_shapetrees(x, slide_idx = slide_idx)
  sp_tree_chr <- vapply(sp_tree, paste, character(1))
  s <- paste(sp_tree_chr, collapse = " ")
  gsub("[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}", "xxx", s) # delete shape's UUIDs
}


test_that("ph_with, phs_with, add_slide create same presentation", {
  x1 <- read_pptx()
  x1 <- add_slide(x1, "Two Content", "Office Theme")
  x1 <- ph_with(x1, "A title", "Title 1")
  x1 <- ph_with(x1, "Jan. 26, 2025", "dt")
  x1 <- ph_with(x1, "Body text", "body [2]")
  x1 <- ph_with(x1, "Footer", 6)

  x2 <- read_pptx()
  x2 <- add_slide(x2, "Two Content", "Office Theme")
  x2 <- phs_with(x2,
    `Title 1` = "A title", dt = "Jan. 26, 2025",
    `body[2]` = "Body text", `6` = "Footer"
  )

  x3 <- read_pptx()
  x3 <- add_slide(x3, "Two Content", "Office Theme",
    `Title 1` = "A title", dt = "Jan. 26, 2025",
    `body[2]` = "Body text", `6` = "Footer"
  )

  st_1 <- get_shapetrees_string(x1)
  st_2 <- get_shapetrees_string(x2)
  st_3 <- get_shapetrees_string(x3)

  expect_equal(st_1, st_2)
  expect_equal(st_1, st_3)
})


test_that("phs_with .slide_idx arg works", {
  x1 <- read_pptx()
  x1 <- add_slide(x1, "Two Content", "Office Theme")
  x1 <- ph_with(x1, "Footer", "ftr")
  x1 <- ph_with(x1, "Jan. 26, 2025", "dt")
  x1 <- add_slide(x1, "Two Content", "Office Theme")
  x1 <- ph_with(x1, "Footer", "ftr")
  x1 <- ph_with(x1, "Jan. 26, 2025", "dt")

  x2 <- read_pptx()
  x2 <- add_slide(x2, "Two Content", "Office Theme")
  x2 <- add_slide(x2, "Two Content", "Office Theme")
  x2 <- phs_with(x2, ftr = "Footer", dt = "Jan. 26, 2025", .slide_idx = 1:2)

  x3 <- read_pptx()
  x3 <- add_slide(x3, "Two Content", "Office Theme")
  x3 <- add_slide(x3, "Two Content", "Office Theme")
  x3 <- phs_with(x3, ftr = "Footer", dt = "Jan. 26, 2025", .slide_idx = "all")

  st_1 <- get_shapetrees_string(x1)
  st_2 <- get_shapetrees_string(x2)
  st_3 <- get_shapetrees_string(x3)

  expect_equal(st_1, st_2)
  expect_equal(st_1, st_3)
})


test_that("... and .dots work in phs_with and add_slide", {
  x1 <- read_pptx()
  x1 <- add_slide(x1, "Two Content", "Office Theme")
  x1 <- phs_with(x1,
    `Title 1` = "A title", dt = "Jan. 26, 2025",
    `body[2]` = "Body text", `6` = "Footer"
  )

  x2 <- read_pptx()
  x2 <- add_slide(x2, "Two Content", "Office Theme")
  x2 <- phs_with(x2, .dots = list(
    `Title 1` = "A title", dt = "Jan. 26, 2025",
    `body[2]` = "Body text", `6` = "Footer"
  ))

  x3 <- read_pptx()
  x3 <- add_slide(x3, "Two Content", "Office Theme")
  x3 <- phs_with(x3,
    `Title 1` = "A title", dt = "Jan. 26, 2025",
    .dots = list(`body[2]` = "Body text", `6` = "Footer")
  )

  x4 <- read_pptx()
  x4 <- add_slide(x4, "Two Content", "Office Theme",
    `Title 1` = "A title", dt = "Jan. 26, 2025",
    .dots = list(`body[2]` = "Body text", `6` = "Footer")
  )

  x5 <- read_pptx()
  x5 <- add_slide(x5, "Two Content", "Office Theme",
    .dots = list(
      `Title 1` = "A title", dt = "Jan. 26, 2025",
      `body[2]` = "Body text", `6` = "Footer"
    )
  )

  st_1 <- get_shapetrees_string(x1)
  st_2 <- get_shapetrees_string(x2)
  st_3 <- get_shapetrees_string(x3)
  st_4 <- get_shapetrees_string(x4)
  st_5 <- get_shapetrees_string(x5)

  expect_equal(st_1, st_2)
  expect_equal(st_1, st_3)
  expect_equal(st_1, st_4)
  expect_equal(st_1, st_5)
})
