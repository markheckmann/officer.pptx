# ____________----
# XML --------------------------------------------


# get shapes from node
# x : node or nodeset
xml_get_shapes <- function(x) {
  xml_find_all(x, ".//p:sp") # // also finds shapes inside groups; maybe only use ".//p:spTree//p:sp"?
}


# get runs
# x : node or nodeset
xml_get_runs <- function(x) {
  xml_find_all(x, ".//a:p/a:r")
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



# ____________----
# PPTX --------------------------------------------

# get slide root node
pptx_slide_get <- function(x, slide_idx) {
  assert_class(x, "rpptx")
  slide <- x$slide$get_slide(slide_idx)
  slide$get()
}


# get all shapes on slide
pptx_shapes_on_slide <- function(x, slide_idx) {
  pptx_slide_get(x, slide_idx) |> xml_get_shapes()
}
