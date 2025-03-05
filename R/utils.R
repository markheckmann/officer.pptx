assert_class <- function(x, cls) {
  if (!inherits(x, cls)) {
    cli::cli_abort("{.arg x} must be an object of type {.val {cls}}")
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
folder_files <- function(path, name = NULL, pattern = ".pptx", fun = NULL) {
  files <- list.files(path, pattern = pattern, full.names = TRUE)
  if (is.null(name)) {
    cli::cli_alert_info("Available files:")
    return(basename(files))
  }
  i <- pmatch(name, basename(files))
  if (is.na(i)) {
    msg <- "No file containing {.val {name}} found."
    if (!is.null(fun)) {
      msg <- c(msg, "x" = "Type {.arg {fun}()} for available files.")
    }
    cli::cli_abort(msg, call = NULL)
  }
  files[i]
}


#' Get path to sample files
#'
#' Run without args (`example_pptx()`) to get a list of available example files.
#'
#' @param name `[character]`\cr Name of file. Partial matches are allowed.
#' @returns Path to PPTX sample file.
#' @export
#' @rdname example-files
#' @examples
#' example_pptx()
#' example_pptx("text_replace")
#'
#' example_image()
#' example_image("dog_1")
example_pptx <- function(name = NULL) {
  path <- system.file("ext/pptx", package = "officer.pptx")
  folder_files(path, name = name, pattern = ".pptx", fun = "example_pptx")
}


#' @export
#' @rdname example-files
example_image <- function(name = NULL) {
  path <- system.file("ext/images", package = "officer.pptx")
  folder_files(path, name = name, pattern = ".png|.jpg", fun = "example_image")
}
