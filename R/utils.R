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


#' Get path to example files
#'
#' Run without args (`example_file()`) to get a list of available example files.
#'
#' @param name `[character]`\cr Name of file. Partial matches are allowed.
#' @export
#' @examples
#' example_file()
#' example_file("example_01")
#'
example_file <- function(name = NULL) {
  path <- system.file("ext", package = "officer.pptx")
  files <- list.files(path, pattern = ".pptx", full.names = TRUE)
  if (is.null(name)) {
    cli::cli_alert_info("Available example files:")
    return(basename(files))
  }
  i <- pmatch(name, basename(files))
  if (is.na(i)) {
    cli::cli_alert_danger("No example file containing {.val {name}}. Type {.arg example_file()} for available files.")
  }
  files[i]
}
