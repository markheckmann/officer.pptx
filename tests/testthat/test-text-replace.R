test_that("text replace work without erros", {
  run_example <- \() {
    file_in <- test_file("testdata_01_replace")
    file_out <- tempfile(fileext = ".pptx")

    x <- read_pptx(file_in)
    x <- pptx_text_replace(x, "@" = "<<>>", "{1}" = "ö")
    print(x, target = file_out)
  }
  expect_no_error(run_example())
})
