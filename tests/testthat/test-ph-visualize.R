# Tests for ph_visualize() function

test_that("ph_visualize adds overlays to all placeholders by default", {
  x <- read_pptx() |>
    add_slide(layout = "Two Content") |>
    ph_visualize()

  # Two Content has 6 placeholders: title, 2x body, dt, ftr, sldNum
  summary <- slide_summary(x)
  expect_equal(nrow(summary), 6)
})


test_that("ph_visualize filters by type string", {
  x <- read_pptx() |>
    add_slide(layout = "Two Content") |>
    ph_visualize(ph = "title")

  summary <- slide_summary(x)
  expect_equal(nrow(summary), 1)
})


test_that("ph_visualize filters by type with index", {
  x <- read_pptx() |>
    add_slide(layout = "Two Content") |>
    ph_visualize(ph = "body[1]")

  summary <- slide_summary(x)
  expect_equal(nrow(summary), 1)

  x <- read_pptx() |>
    add_slide(layout = "Two Content") |>
    ph_visualize(ph = "body[2]")

  summary <- slide_summary(x)
  expect_equal(nrow(summary), 1)
})


test_that("ph_visualize accepts multiple ph specs", {
  x <- read_pptx() |>
    add_slide(layout = "Two Content") |>
    ph_visualize(ph = c("title", "body[1]"))

  summary <- slide_summary(x)
  expect_equal(nrow(summary), 2)

  x <- read_pptx() |>
    add_slide(layout = "Two Content") |>
    ph_visualize(ph = c("body[1]", "body[2]"))

  summary <- slide_summary(x)
  expect_equal(nrow(summary), 2)
})


test_that("ph_visualize accepts ph_location_type", {
  x <- read_pptx() |>
    add_slide(layout = "Title and Content") |>
    ph_visualize(ph = ph_location_type("body"))

  summary <- slide_summary(x)
  expect_equal(nrow(summary), 1)
})


test_that("ph_visualize accepts ph_location_label", {
  x <- read_pptx() |>
    add_slide(layout = "Two Content") |>
    ph_visualize(ph = ph_location_label("Content Placeholder 2"))

  summary <- slide_summary(x)
  expect_equal(nrow(summary), 1)
})


test_that("ph_visualize with fill adds colored background", {
  x <- read_pptx() |>
    add_slide(layout = "Title and Content") |>
    ph_visualize(ph = "body", background = "#FFE4E1")

  summary <- slide_summary(x)
  expect_equal(nrow(summary), 1)
})


test_that("ph_visualize with line = NULL adds only fill",
{
  x <- read_pptx() |>
    add_slide(layout = "Title and Content") |>
    ph_visualize(ph = "body", background = "lightblue", line = NULL)

  summary <- slide_summary(x)
  expect_equal(nrow(summary), 1)
})


test_that("ph_visualize with label adds text overlays", {
  x <- read_pptx() |>
    add_slide(layout = "Two Content") |>
    ph_visualize(ph = "title", label = TRUE)

  # 1 for border/fill + 1 for label text
  summary <- slide_summary(x)
  expect_equal(nrow(summary), 2)
})


test_that("ph_visualize with label only (no fill, no line)", {
  x <- read_pptx() |>
    add_slide(layout = "Two Content") |>
    ph_visualize(ph = "title", background = NULL, line = NULL, label = TRUE)

  # Only label text, no border/fill overlay
  summary <- slide_summary(x)
  expect_equal(nrow(summary), 1)
})


test_that("ph_visualize respects slide_idx parameter", {
  x <- read_pptx() |>
    add_slide(layout = "Title and Content") |>
    add_slide(layout = "Two Content") |>
    ph_visualize(slide_idx = 1, ph = "body")

  expect_equal(nrow(slide_summary(x, 1)), 1)
  expect_equal(nrow(slide_summary(x, 2)), 0)
})


test_that("ph_visualize warns when no options specified", {
  expect_warning(
    read_pptx() |>
      add_slide(layout = "Two Content") |>
      ph_visualize(background = NULL, line = NULL, label = FALSE),
    "No visualization options"
  )
})


test_that("ph_visualize warns for non-existent placeholder", {
  expect_warning(
    read_pptx() |>
      add_slide(layout = "Two Content") |>
      ph_visualize(ph = "nonexistent_placeholder"),
    "No matching placeholders"
  )
})


test_that("ph_visualize handles 'left' and 'right' specifiers", {
  x <- read_pptx() |>
    add_slide(layout = "Two Content") |>
    ph_visualize(ph = "left")

  summary <- slide_summary(x)
  expect_equal(nrow(summary), 1)

  x <- read_pptx() |>
    add_slide(layout = "Two Content") |>
    ph_visualize(ph = "right")

  summary <- slide_summary(x)
  expect_equal(nrow(summary), 1)
})


test_that("ph_visualize warns for 'fullsize' specifier", {
  expect_warning(
    expect_warning(
      read_pptx() |>
        add_slide(layout = "Two Content") |>
        ph_visualize(ph = "fullsize"),
      "fullsize"
    ),
    "No matching placeholders"
  )
})


test_that("transparent PNG is shipped with package", {
  png_path <- system.file("ext/images/transparent.png", package = "officer.pptx")
  expect_true(file.exists(png_path))
})
