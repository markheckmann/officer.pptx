between <- getFromNamespace("between", "officer")
pptx_fortify_slides <- getFromNamespace("between", "officer")


#' @export
#' @title Clone a slide
#' @description Duplicate a slide within a presentation. The cloned
#' slide is inserted directly after the original slide by default.
#' @param x an `rpptx` object.
#' @param index slide index to clone. Defaults to the current slide.
#' @param where location for the new slide, either `"after"` to insert
#'   immediately after the original or `"last"` to append it at the end.
#' @return the modified `rpptx` object.
#' @family slide_manipulation
clone_slide <- function(x, index = NULL, where = c("after", "last")) {
  where <- match.arg(where)
  if (is.null(index)) {
    index <- x$cursor
  }

  # Write slides and associated notes to disk so that
  # the cloned slide contains all modifications that
  # may have been made in memory.
  x$slide$save_slides()
  x$notesSlide$save_slides()

  l_ <- length(x)
  if (l_ < 1) {
    stop("presentation contains no slide", call. = FALSE)
  }
  if (!between(index, 1, l_)) {
    stop("unvalid index ", index, " (", l_, " slide(s))", call. = FALSE)
  }

  slide_info <- x$presentation$slide_data()
  src_target <- slide_info$target[index]
  src_file <- file.path(x$package_dir, "ppt", src_target)
  new_name <- x$slide$get_new_slidename()
  new_file <- file.path(x$package_dir, "ppt/slides", new_name)
  file.copy(src_file, new_file, overwrite = TRUE, copy.mode = FALSE)

  rel_file <- file.path(dirname(src_file), "_rels", paste0(basename(src_file), ".rels"))
  if (file.exists(rel_file)) {
    new_rel_file <- file.path(dirname(new_file), "_rels", paste0(basename(new_file), ".rels"))
    dir.create(dirname(new_rel_file), recursive = TRUE, showWarnings = FALSE)
    file.copy(rel_file, new_rel_file, overwrite = TRUE, copy.mode = FALSE)

    rel_doc <- read_xml(new_rel_file)
    rel_doc <- fix_rel_paths(rel_doc)
    notes_rel <- xml_find_first(
      rel_doc,
      "//*[local-name()='Relationship' and @Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/notesSlide']"
    )
    if (!inherits(notes_rel, "xml_missing")) {
      notes_src <- xml_attr(notes_rel, "Target")
      notes_file <- file.path(x$package_dir, "ppt/notesSlides", basename(notes_src))
      if (file.exists(notes_file)) {
        new_notes <- x$notesSlide$get_new_slidename()
        new_notes_file <- file.path(x$package_dir, "ppt/notesSlides", new_notes)
        file.copy(notes_file, new_notes_file, overwrite = TRUE, copy.mode = FALSE)

        notes_rel_file <- file.path(dirname(notes_file), "_rels", paste0(basename(notes_file), ".rels"))
        if (file.exists(notes_rel_file)) {
          new_notes_rel_file <- file.path(dirname(new_notes_file), "_rels", paste0(basename(new_notes_file), ".rels"))
          dir.create(dirname(new_notes_rel_file), recursive = TRUE, showWarnings = FALSE)
          file.copy(notes_rel_file, new_notes_rel_file, overwrite = TRUE, copy.mode = FALSE)

          notes_rel_doc <- read_xml(new_notes_rel_file)
          notes_rel_doc <- fix_rel_paths(notes_rel_doc)
          slide_rel <- xml_find_first(
            notes_rel_doc,
            "//*[local-name()='Relationship' and @Type='http://schemas.openxmlformats.org/officeDocument/2006/relationships/slide']"
          )
          if (!inherits(slide_rel, "xml_missing")) {
            xml_attr(slide_rel, "Target") <- file.path("../slides", new_name, fsep = "/")
          }
          write_xml(notes_rel_doc, new_notes_rel_file)
        }

        xml_attr(notes_rel, "Target") <- file.path("../notesSlides", new_notes, fsep = "/")

        x$content_type$add_notesSlide(partname = file.path("/ppt/notesSlides", new_notes, fsep = "/"))
        x$notesSlide$add_slide(new_notes_file)

        idx_notes <- x$notesSlide$slide_index(new_notes)
        nslide <- x$notesSlide$get_slide(idx_notes)
        nslide$fortify_id()
      }
    }
    write_xml(rel_doc, new_rel_file)
  }

  x$presentation$add_slide(target = file.path("slides", new_name))
  x$content_type$add_slide(partname = file.path("/ppt/slides", new_name))
  x$slide$add_slide(new_file, x$slideLayouts$get_xfrm_data())

  new_index <- x$slide$length()
  x$cursor <- new_index

  if (where == "after") {
    x <- move_slide(x, index = new_index, to = index + 1)
  }

  x <- pptx_fortify_slides(x)
  x <- pptx_fortify_notes(x)
  x
}


#' @noRd
pptx_fortify_notes <- function(x) {
  for (i in seq_len(x$notesSlide$length())) {
    nslide <- x$notesSlide$get_slide(i)
    nslide$fortify_id()
  }
  x
}

#' @noRd
fix_rel_paths <- function(doc) {
  rels <- xml_find_all(doc, "//*[local-name()='Relationship']")
  for (rel in rels) {
    tgt <- xml_attr(rel, "Target")
    if (!is.na(tgt)) {
      xml_attr(rel, "Target") <- gsub("\\\\", "/", tgt)
    }
  }
  doc
}
