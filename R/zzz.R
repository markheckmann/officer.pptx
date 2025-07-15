.onLoad <- function(libname, pkgname) {
  # temp dir pdf conversions for speedups
  options(
    officer.pptx.warm_convert = FALSE,
    officer.pptx.pdf_storage = create_pdf_storage_folder()
  )

  # register new methods for img class for officer's generics
  registerS3method("ph_with", "img", ph_with.img, envir = asNamespace("officer"))
  # registerS3method("to_pml", "img", to_pml.img, envir = asNamespace("officer"))
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
