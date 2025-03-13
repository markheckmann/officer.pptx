assert_class <- function(x, cls) {
  if (!inherits(x, cls)) {
    cli::cli_abort("{.arg x} must be an object of type {.val {cls}}")
  }
}


assert_pkg_namespace <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort("Function requires {.pkg {pkg}} to be installed.")
  }
}


# coalesce for R
`%||%` <- function(l, r) {
  if (is.null(l)) r else l
}


# not in
`%nin%` <- Negate(`%in%`)



# In OOXML, shape rotations are expressed in units of 1/60000th of a degree.
# For example:
#   1° becomes 1 × 60000 = 60000
# 90° becomes 90 × 60000 = 5400000
# This conversion factor allows precise rotation settings in OOXML documents.
degree_to_rotation <- \(x) {
  as.integer(x * 60000)
}


rotation_to_degree <- \(x) {
  x / 60000
}


# as_integer(1)
# as_integer(1.1)
# as_integer(1.0)
# as_integer(c(1.0, 2.0))
as_integer <- \(x, name = "x") {
  if (!is_integerish(x)) {
    cli::cli_alert_warning("{.arg name} is no {.cls integer}. Casting to integer")
  }
  as.integer(x)
}

# is_integerish(1)
# is_integerish(1.0)
# is_integerish(c(1.0, 2.0))
is_integerish <- function(x) {
  ii <- all(is.numeric(x) | is.integer(x))
  jj <- all(x == as.integer(x))
  ii && jj
}

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


# check if LibreOffice is installed
soffice_available <- function() {
  soffice_path <- Sys.which("soffice")
  nzchar(soffice_path) > 0
}
