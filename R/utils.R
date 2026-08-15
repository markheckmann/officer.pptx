assert_class <- function(x, cls) {
  if (!inherits(x, cls)) {
    cli::cli_abort("{.arg x} must be an object of type {.val {cls}}")
  }
}


assert_pkg_namespace <- function(pkg, fun = NULL) {
  fun_str <- ifelse(is.null(fun), "Function", "{.fn {fun}}")
  mgs <- paste(fun_str, "requires package {.pkg {pkg}} to be installed.")
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort(mgs, call = NULL)
  }
}


# check several namespaces at once
requireNamespaces <- function(pkgs, fail_if_missing = FALSE, quietly = TRUE) {
  if (!is.character(pkgs)) {
    cli::cli_abort("{.arg pkgs} must be a {.cls character}, not {.cls {class(pkgs)[1]}}")
  }
  pkg_status <- vapply(pkgs, requireNamespace, quietly = quietly, USE.NAMES = TRUE, FUN.VALUE = logical(1))
  if (fail_if_missing && any(!pkg_status)) {
    missed <- names(pkg_status[!pkg_status])
    cli::cli_abort("{cli::qty(missed)} Package{?s} {.pkg {missed}} not installed.", call = NULL)
  }
  pkg_status
}



# coalesce for R
`%||%` <- function(l, r) {
  if (is.null(l)) r else l
}


# not in
`%nin%` <- Negate(`%in%`)


# Base-R equivalents of stringr functions (fixed pattern only)

str_count_fixed <- function(x, pattern) {
  m <- gregexpr(pattern, x, fixed = TRUE)
  vapply(m, function(mi) sum(mi > 0L), integer(1))
}

str_locate_all_fixed <- function(x, pattern) {
  m <- gregexpr(pattern, x, fixed = TRUE)[[1]]
  if (m[1] == -1L) {
    return(matrix(integer(0), ncol = 2, dimnames = list(NULL, c("start", "end"))))
  }
  starts <- as.integer(m)
  ends <- starts + attr(m, "match.length") - 1L
  cbind(start = starts, end = ends)
}



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


get_shapetree <- function(x, slide_idx = NULL) {
  stop_if_not_rpptx(x)
  slide_idx <- slide_idx %||% x$cursor
  xml_node <- x$slide$get_slide(slide_idx)$get()
  xml2::xml_child(xml_node, "*/p:spTree")
}


get_shapetrees <- function(x, slide_idx = NULL) {
  stop_if_not_rpptx(x)
  slide_idx <- slide_idx %||% seq_len(length(x))
  lapply(slide_idx, function(idx) get_shapetree(x, idx))
}


# all slide's shapetrees as a string and shape's UUIDs removed
# used to check if created slides are identical.
get_shapetrees_string <- function(x, slide_idx = NULL) {
  stop_if_not_rpptx(x)
  sp_tree <- get_shapetrees(x, slide_idx = slide_idx)
  sp_tree_chr <- vapply(sp_tree, paste, character(1))
  s <- paste(sp_tree_chr, collapse = " ")
  gsub("[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}", "xxx", s) # delete shape's UUIDs
}


# From other packages -------------------------------------------------

# tools::file_ext
file_ext <- function(x) {
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}

# Mini versions  -------------------------------------------------

#
# name <- "Alice"
# x     <- 3.14
# mini_glue("Hello, {name}!  x ≈ {round(x, 1)}")
# #> "Hello, Alice!  x ≈ 3.1"
#
# # multiple strings at once
# mini_glue(c("a = {1+1}", "today is '{Sys.Date()}'"))
# #> c("a = 2", "today is 2025-06-23")
# #>
mini_glue <- function(x, ..., .envir = parent.frame()) {
  # x: character vector of template strings
  # ...: named values to inject
  # .envir: fallback environment for evaluation

  # coerce to character
  x <- as.character(x)
  # collect named args and build a child env
  args <- list(...)
  eval_env <- list2env(args, parent = .envir)

  # regex for {expr}
  pat <- "\\{([^{}]+)\\}"

  sapply(x, function(str) {
    # find all "{…}" in this string
    locs <- gregexpr(pat, str, perl = TRUE)
    matches <- regmatches(str, locs)[[1]]

    if (length(matches) == 0) {
      return(str)
    }

    # eval each match inside eval_env
    replacements <- vapply(matches, function(m) {
      expr_text <- sub("^\\{([^{}]+)\\}$", "\\1", m)
      val <- eval(parse(text = expr_text), envir = eval_env)
      paste(val, collapse = " ")
    }, character(1))

    # do the substitution
    regmatches(str, locs) <- list(replacements)
    str
  }, USE.NAMES = FALSE)
}
