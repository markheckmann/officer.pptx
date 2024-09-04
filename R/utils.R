assert_class <- function(x, class) {
  if (!inherits(x, class)) {
    cli::cli_abort("{.arg x} must be an object of type {.val {class}}")
  }
}


# coalesce for R
`%||%` <- function(l, r) {
  if (is.null(l)) r else l
}


# not in
`%nin%` <- Negate(`%in%`)


#' Open local file
#' @param path Path to file.
#' @export
#' @examples \dontrun{
#' file <- system.file("img", "Rlogo.png", package = "png")
#' file_open(file)
#' }
#'
file_open <- function(path) {
  path <- normalizePath(path)
  path_quoted <- shQuote(path)
  os_type <- .Platform$OS.type
  if (os_type == "windows") {
    shell.exec(path)
  } else {
    if (Sys.info()["sysname"] == "Darwin") {
      system(paste("open", path_quoted))
    } else {
      system(paste("xdg-open", path_quoted))
    }
  }
}


# path: path to file folder
folder_files <- function(path, name = NULL, pattern = ".pptx", fun = "example_file") {
  files <- list.files(path, pattern = pattern, full.names = TRUE)
  if (is.null(name)) {
    cli::cli_alert_info("Available files:")
    return(basename(files))
  }
  i <- pmatch(name, basename(files))
  if (is.na(i)) {
    cli::cli_alert_danger("No file containing {.val {name}} found. Type {.arg {fun}()} for available files.")
  }
  files[i]
}


#' Get path to example files
#'
#' Run without args (`example_file()`) to get a list of available example files.
#'
#' @param name `[character]`\cr Name of file. Partial matches are allowed.
#' @returns Path to PPTX sample file.
#' @export
#' @examples
#' example_file()
#' example_file("text_replace")
#'
example_file <- function(name = NULL) {
  path <- system.file("ext", package = "officer.pptx")
  folder_files(path, name = name, pattern = ".pptx", fun = "example_file")
}


#' Get path to test files
#'
#' Run without args (`test_file()`, `test_image()`) to get a list of available test files.
#' @inheritParams example_file
#' @returns Path to PPTX test file.
#' @export
#' @keywords internal
#' @rdname test-files
#' @examples
#' test_file()
#' test_file("testdata_01")
#'
#' test_image()
#' test_image("flag_de")
#'
test_file <- function(name = NULL) {
  path <- system.file("ext/testdata", package = "officer.pptx")
  folder_files(path, name = name, pattern = ".pptx", fun = "test_file")
}


#' @export
#' @keywords internal
#' @rdname test-files
test_image <- function(name = NULL) {
  path <- system.file("ext/testimages", package = "officer.pptx")
  folder_files(path, name = name, pattern = ".png|.jpeg|.jpg", fun = "test_file")
}
