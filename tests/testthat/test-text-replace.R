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
