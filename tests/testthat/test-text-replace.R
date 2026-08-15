test_that("text_replace works without errors", {
  file_in <- test_file("testdata_01_replace")
  file_out <- tempfile(fileext = ".pptx")

  x <- read_pptx(file_in)
  x <- text_replace(x, "@" = "<<>>", "{1}" = "ö", verbose = 0L)
  print(x, target = file_out)
  expect_true(file.exists(file_out))
})


test_that("text_replace_log returns replacement tibble", {
  file_in <- test_file("testdata_01_replace")
  x <- read_pptx(file_in)
  x <- text_replace(x, "@" = "<<>>", "{1}" = "ö", verbose = 0L)

  log <- text_replace_log(x)
  expect_s3_class(log, "tbl_df")
  expect_named(log, c("slide_idx", "shape_name", "pattern", "replacement", "count"))
  expect_true(all(log$count > 0L))
})


test_that("text_replace_log returns NULL when no text_replace was run", {
  file_in <- test_file("testdata_01_replace")
  x <- read_pptx(file_in)
  expect_null(text_replace_log(x))
})


test_that("text_replace dry_run does not modify content", {
  file_in <- test_file("testdata_01_replace")
  x <- read_pptx(file_in)

  x_dry <- text_replace(x, "@" = "REPLACED", verbose = 0L, dry_run = TRUE)
  log <- text_replace_log(x_dry)
  expect_true(nrow(log) > 0L)

  x_after <- text_replace(x_dry, "@" = "REPLACED", verbose = 0L, dry_run = TRUE)
  log_after <- text_replace_log(x_after)
  expect_equal(log$count, log_after$count)
})


test_that("text_replace verbose prints output", {
  file_in <- test_file("testdata_01_replace")
  x <- read_pptx(file_in)
  expect_message(
    text_replace(x, "@" = ">>>", verbose = 1L),
    "text_replace"
  )
})


test_that("text_replace_expect passes on correct count", {
  file_in <- test_file("testdata_01_replace")
  x <- read_pptx(file_in)
  x <- text_replace(x, "@" = ">>>", verbose = 0L)
  log <- text_replace_log(x)
  total <- sum(log$count[log$pattern == "@"])

  expect_no_error(text_replace_expect(x, "@", n = total))
  expect_no_error(text_replace_expect(x, "@", min = 1L))
  expect_no_error(text_replace_expect(x, "@", max = total + 1L))
})


test_that("text_replace_expect fails on wrong count", {
  file_in <- test_file("testdata_01_replace")
  x <- read_pptx(file_in)
  x <- text_replace(x, "@" = ">>>", verbose = 0L)

  expect_error(text_replace_expect(x, "@", n = 0L))
  expect_error(text_replace_expect(x, "@", min = 9999L))
  expect_error(text_replace_expect(x, "@", max = 0L))
})


test_that("text_replace_expect works with slide_idx filter", {
  file_in <- test_file("testdata_01_replace")
  x <- read_pptx(file_in)
  x <- text_replace(x, "@" = ">>>", verbose = 0L)
  log <- text_replace_log(x)

  slide_1_count <- sum(log$count[log$pattern == "@" & log$slide_idx == 1L])
  if (slide_1_count > 0L) {
    expect_no_error(text_replace_expect(x, "@", min = 1L, slide_idx = 1L))
  }
  expect_no_error(text_replace_expect(x, "@", n = 0L, slide_idx = 9999L))
})


test_that("text_replace_expect errors without log", {
  file_in <- test_file("testdata_01_replace")
  x <- read_pptx(file_in)
  expect_error(text_replace_expect(x, "@", min = 1L), "No text replacement log")
})


test_that("text_replace_expect errors when n combined with min/max", {
  file_in <- test_file("testdata_01_replace")
  x <- read_pptx(file_in)
  x <- text_replace(x, "@" = ">>>", verbose = 0L)
  expect_error(text_replace_expect(x, "@", n = 1L, min = 1L), "cannot be used together")
})


test_that("text_replace with no matches returns empty log", {
  file_in <- test_file("testdata_01_replace")
  x <- read_pptx(file_in)
  x <- text_replace(x, "ZZZNONEXISTENT" = "foo", verbose = 0L)
  log <- text_replace_log(x)
  expect_equal(nrow(log), 0L)
})


test_that("text_replace ph_type includes only matching placeholders", {
  file_in <- example_pptx("text_replace")
  x <- read_pptx(file_in)
  x <- text_replace(x, "{1}" = "X", ph_type = "title", slide_idx = 1, verbose = 0L)
  log <- text_replace_log(x)
  expect_true(nrow(log) > 0L)
  expect_true(all(log$shape_name == "Titel 3"))
})


test_that("text_replace ph_type excludes non-placeholder shapes", {
  file_in <- example_pptx("text_replace")
  x <- read_pptx(file_in)
  x <- text_replace(x, "{1}" = "X", ph_type = "title", slide_idx = 1, verbose = 0L)
  log <- text_replace_log(x)
  expect_false(any(log$shape_name %in% c("Textplatzhalter 3", "Rechteck 5", "Rechteck 6")))
})


test_that("text_replace exclude_ph_type removes matching placeholders", {
  file_in <- example_pptx("text_replace")
  x <- read_pptx(file_in)
  x <- text_replace(x, "{1}" = "X", exclude_ph_type = "title", slide_idx = 1, verbose = 0L)
  log <- text_replace_log(x)
  expect_false(any(log$shape_name == "Titel 3"))
  expect_true(nrow(log) > 0L)
})


test_that("text_replace exclude_ph_type keeps non-placeholder shapes", {
  file_in <- example_pptx("text_replace")
  x <- read_pptx(file_in)
  x <- text_replace(x, "[]" = "X", exclude_ph_type = "title", slide_idx = 2, verbose = 0L)
  log <- text_replace_log(x)
  expect_true(any(log$shape_name == "Rechteck 6"))
})


test_that("text_replace works with bracket placeholders", {
  file_in <- test_file("testdata_01_replace")
  x <- read_pptx(file_in)
  x <- text_replace(x, "[2]" = "A", slide_idx = 2, verbose = 0L)
  log <- text_replace_log(x)
  expect_true(sum(log$count) >= 1L)

  shapes <- pptx_shapes_on_slide(x, 2)
  texts <- vapply(shapes, xml2::xml_text, character(1))
  expect_false(any(grepl("[2]", texts, fixed = TRUE)))
})


test_that("text_replace works with empty bracket placeholder", {
  file_in <- test_file("testdata_01_replace")
  x <- read_pptx(file_in)
  x <- text_replace(x, "[]" = "EMPTY", slide_idx = 2, verbose = 0L)
  log <- text_replace_log(x)
  expect_true(sum(log$count) >= 1L)

  shapes <- pptx_shapes_on_slide(x, 2)
  texts <- vapply(shapes, xml2::xml_text, character(1))
  expect_false(any(grepl("[]", texts, fixed = TRUE)))
})


test_that("text_replace longer pattern first avoids substring collision", {
  file_in <- test_file("testdata_01_replace")
  x <- read_pptx(file_in)
  x <- x |>
    text_replace("[[2]]" = "DOUBLE", slide_idx = 2, verbose = 0L) |>
    text_replace("[2]" = "SINGLE", slide_idx = 2, verbose = 0L)
  shapes <- pptx_shapes_on_slide(x, 2)
  title_text <- xml2::xml_text(shapes[[1]])
  expect_true(grepl("DOUBLE", title_text, fixed = TRUE))
  expect_true(grepl("SINGLE", title_text, fixed = TRUE))
})


test_that("text_replace replaces across both slides", {
  file_in <- test_file("testdata_01_replace")
  x <- read_pptx(file_in)
  x <- text_replace(x, "{1}" = "A", "[2]" = "B", verbose = 0L)
  log <- text_replace_log(x)
  expect_true(any(log$slide_idx == 1L))
  expect_true(any(log$slide_idx == 2L))

  shapes_1 <- pptx_shapes_on_slide(x, 1)
  texts_1 <- vapply(shapes_1, xml2::xml_text, character(1))
  expect_false(any(grepl("{1}", texts_1, fixed = TRUE)))

  shapes_2 <- pptx_shapes_on_slide(x, 2)
  texts_2 <- vapply(shapes_2, xml2::xml_text, character(1))
  expect_false(any(grepl("[2]", texts_2, fixed = TRUE)))
})


test_that("text_replace slide_idx limits to single slide only", {
  file_in <- test_file("testdata_01_replace")
  x <- read_pptx(file_in)
  x <- text_replace(x, "{1}" = "A", slide_idx = 1, verbose = 0L)
  log <- text_replace_log(x)
  expect_true(all(log$slide_idx == 1L))

  shapes_2 <- pptx_shapes_on_slide(x, 2)
  texts_2 <- vapply(shapes_2, xml2::xml_text, character(1))
  expect_true(any(grepl("[2]", texts_2, fixed = TRUE)))
})
