#' Get path to test files
#'
#' Run without args (`test_file()`, `test_image()`) to get a list of available test files.
#' @inheritParams example_pptx
#' @returns Path to PPTX test file.
#' @export
#' @keywords internal
#' @noRd
#' @examples
#' test_file()
#' test_file("testdata_01")
#'
#' test_image()
#' test_image("flag_de")
#'
test_file <- function(name = NULL) {
  path <- test_path("testdata")
  folder_files(path, name = name, pattern = ".pptx", fun = "test_file")
}


#' @export
#' @keywords internal
#' @noRd
test_image <- function(name = NULL) {
  path <- test_path("testimages")
  folder_files(path, name = name, pattern = ".png|.jpeg|.jpg|.wmf", fun = "test_image")
}
