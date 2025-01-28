# ____________----
# HELPERS ---------------------------------------------

# EMUs (English Metric Unit)

emu_to_inch <- function(x) {
  x / 914400
}


emu_to_cm <- function(x) {
  x / 360000
}

cm_to_inch <- function(x) {
  x * .39370078740157
}


cm_to_emu <- function(x) {
  x * 360000
}


# ____________----
# XML --------------------------------------------

### runs ----

# get runs
# x : node or nodeset
xml_get_runs <- function(x) {
  xml_find_all(x, ".//a:p/a:r")
}

### images ----

# get images from node
# x : node or nodeset
xml_get_images <- function(x) {
  xml_find_all(x, ".//p:pic") # // also finds shapes inside groups; maybe only use ".//p:spTree//p:sp"?
}

### shapes ----


# get shapes from node
# x : node or nodeset
xml_get_shapes <- function(x) {
  xml_find_all(x, ".//p:sp") # // also finds shapes inside groups; maybe only use ".//p:spTree//p:sp"?
}





# filter shapes by text pattern
# x : node or nodeset
# returns: shape nodes
xml_shapes_filter <- function(shapes, pattern = NULL) {
  if (is.null(pattern)) {
    return(shapes)
  }
  texts <- shapes |> xml_text()
  ii <- stringr::str_detect(texts, stringr::fixed(pattern))
  shapes[ii]
}



# find shapes by text pattern
# x : node or nodeset
# returns: shape nodes
xml_shapes_find <- function(x, pattern) {
  shapes <- x |> xml_get_shapes()
  texts <- shapes |> xml_text()
  ii <- stringr::str_detect(texts, stringr::fixed(pattern))
  shapes[ii]
}


# x : node or nodeset
xml_is_shape <- function(x) {
  all(x |> xml_name() == "sp")
}


# hide shape
xml_shape_hide <- function(x) {
  # set attribute hidden = "1"
  # <p:cNvPr id="6" name="Rechteck 6" hidden="1">
  c_nv_pr <- xml_child(x, "p:nvSpPr/p:cNvPr")
  xml_attr(c_nv_pr, attr = "hidden") <- 1
}


# unhide shape
xml_shape_unhide <- function(x) {
  c_nv_pr <- xml_child(x, "p:nvSpPr/p:cNvPr")
  xml_attr(c_nv_pr, attr = "hidden") <- 0
}


# get hidden attribute of non-visible shape props
xml_shape_attr_hidden <- function(shapes) {
  # set attribute hidden = "1"
  # <p:cNvPr id="6" name="Rechteck 6" hidden="1">
  c_nv_pr <- xml_child(shapes, "p:nvSpPr/p:cNvPr")
  xml_attr(c_nv_pr, attr = "hidden")
}


# remove a shape
xml_shape_remove <- function(shape) {
  # is shape
  xml_remove(shape)
}


read_xfrm <- getFromNamespace("read_xfrm", ns = "officer")

# get xfrm info from shape node
# xfrm into: http://officeopenxml.com/drwSp-size.php
# TODO: placeholders to not always have xrfm
xml_shape_get_frame <- function(shape) {
  l <- read_xfrm(shape, "", NA) |> unlist()
  p <- l[c("offx", "offy", "cx", "cy")] |>
    as.numeric() |>
    emu_to_inch() |>
    setNames(c("left", "top", "width", "height")) |>
    as.list()
  do.call(frame, p)
}


# extract infos form a shape
xml_shape_info <- function(shape) {
  nvpr <- shape |> xml2::xml_find_first(".//p:cNvPr")
  id <- nvpr |> xml2::xml_attr("id")
  name <- nvpr |> xml2::xml_attr("name")
  text <- shape |> xml_text()
  ph <- shape |> xml_is_placeholder()

  prst_geom <- shape |> xml2::xml_find_first(".//a:prstGeom") # Preset Geometry
  geom <- prst_geom |> xml2::xml_attr("prst")

  list(id = id, ph = ph, name = name, text = text, geom = geom)
}


xml_is_placeholder <- function(node) {
  !is.na(node |> xml_find_first(".//p:nvPr/p:ph"))
}


xml_placeholder_type <- function(node) {
  node |>
    xml_find_first(".//p:nvPr/p:ph") |>
    xml_attr("type")
}


# ____________----
# PPTX --------------------------------------------

# get slide root node
pptx_slide_get <- function(x, slide_idx) {
  assert_class(x, "rpptx")
  slide <- x$slide$get_slide(slide_idx)
  slide$get()
}


# get all shapes on slide
pptx_shapes_on_slide <- function(x, slide_idx, pattern = NULL) {
  shapes <- pptx_slide_get(x, slide_idx) |> xml_get_shapes()
  shapes |> xml_shapes_filter(pattern)
}


#' Hide und unhide shapes that match pattern
#' @param x `[rpptx]`\cr An [officer] object. See [read_pptx()].
#' @param pattern `[character]`\cr Vector with patterns to find shapes to hide/unhide. Regex is not interpreted.
#' @param slide_idx `[numeric]`\cr Index of slides to process. If `NULL` (default), all slides
#' @export
#' @rdname shapes-hide-unhide
pptx_shapes_hide_temp <- function(x, pattern, slide_idx = NULL) {
  .pptx_hide_unhide_shapes("hide", x, pattern, slide_idx)
}


#' @export
#' @rdname shapes-hide-unhide
pptx_shapes_unhide_temp <- function(x, pattern, slide_idx = NULL) {
  .pptx_hide_unhide_shapes("unhide", x, pattern, slide_idx)
}


.pptx_hide_unhide_shapes <- function(action = "hide", x, pattern, slide_idx = NULL, ...) {
  assert_class(x, "rpptx")
  action <- match.arg(action, c("hide", "unhide"))
  .fun <- switch(action,
    hide = xml_shape_hide,
    unhide = xml_shape_unhide
  )
  slide_idx <- slide_idx %||% seq_along(x)
  for (s_idx in slide_idx) {
    x$cursor <- s_idx # necessary for officer functions
    for (i in seq_along(pattern)) {
      shapes <- x |> pptx_shapes_on_slide(slide_idx = s_idx, pattern = pattern[i])
      .fun(shapes)
    }
  }
  x
}


# get all shapes on slide
pptx_images_on_slide <- function(x, slide_idx, pattern = NULL) {
  pptx_slide_get(x, slide_idx) |> xml_get_images()
  # shapes |> xml_shapes_filter(pattern)
}




# extract size and position from shape and convert to frame
frame_from_shape <- function(shape) {
  l <- xml_shape_get_frame(shape)
  do.call(frame, l)
}


# shapes objects plus additional information
pptx_shapes_info <- function(x, slide_idx = NULL, unnest = TRUE, as_list = FALSE, ...) {
  assert_class(x, "rpptx")
  slide_idx <- slide_idx %||% seq_len(x$slide$length())
  nodesets <- lapply(slide_idx, \(idx) pptx_shapes_on_slide(x, idx))
  nodes <- lapply(nodesets, \(ns) lapply(ns, identity))

  df <- tibble(slide_idx = slide_idx, type = "shape", node = nodes) |> unnest(node)
  df$info <- lapply(df$node, xml_shape_info)
  df$frame <- lapply(df$node, frame_from_shape)
  if (unnest) {
    df <- df |> unnest_wider(c(frame, info))
  }
  if (as_list) {
    df <- df |> purrr::transpose()
  }
  df
}


# image info plus additional information
pptx_images_info <- function(x, slide_idx = NULL, unnest = TRUE, as_list = FALSE, ...) {
  assert_class(x, "rpptx")
  slide_idx <- slide_idx %||% seq_len(x$slide$length())
  nodesets <- lapply(slide_idx, \(idx) pptx_images_on_slide(x, idx))
  nodes <- lapply(nodesets, \(ns) lapply(ns, identity))

  df <- tibble(slide_idx = slide_idx, type = "image", node = nodes) |> unnest(node)
  df$info <- lapply(df$node, xml_shape_info)
  df$frame <- lapply(df$node, frame_from_shape)
  if (unnest) {
    df <- df |> unnest_wider(c(frame, info))
  }
  if (as_list) {
    df <- df |> purrr::transpose()
  }
  df
}
