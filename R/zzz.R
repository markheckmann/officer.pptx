.onLoad <- function(libname, pkgname) {
  # temp dir pdf conversions for speedups
  options(
    officer.pptx.warm_convert = FALSE,
    officer.pptx.pdf_storage = create_pdf_storage_folder()
  )
}


create_pdf_storage_folder <- function() {
  pdf_storage_path <- file.path(tempdir(), "pdf_storage")
  if (!dir.exists(pdf_storage_path)) {
    dir.create(pdf_storage_path, showWarnings = FALSE)
  }
  pdf_storage_path
}


get_pdf_storage_folder <- function() {
  options()$officer.pptx.pdf_storage
}
