library(officer)

devtools::load_all()

file_in <- "/Users/markheckmann/Library/CloudStorage/SynologyDrive-DPG-Modellvorhaben/02 Arbeitspakete Modellvorhaben/05 Arbeitspaket 5_Strukturaufbau und Bedarfsanalyse/02 Bedarfsanalyse Beschäftigte/Mitarbeiterbefragung/Auswertungen/Präventionsrat/Präventionsrat V09 2024-05-27.pptx"

file_in <- test_file("03")
file_open(file_in)
x <- pptx_read(file_in)
# file_out <- "dev_local/04_shape_info/image_insert_FILLED.pptx"
# image <- "dev_local/02_insert_image/dog.jpeg"

# placeholder does not seem to have an xfrm, so no coordinates
# it would be nicer, if the placeholder coords are also displayed here
# if shape contains image not shown here
df <- pptx_shapes_info(x)
df <- pptx_images_info(x)

xml_is_placeholder <- function(node) {
  !is.na(node |> xml_find_first(".//p:nvPr/p:ph"))
}


xml_placeholder_type <- function(node) {
  node |> xml_find_first(".//p:nvPr/p:ph") |> xml_attr("type")
}

xml_is_placeholder(node)

node <- df$node[[1]]
ph <-



# get image name and path
df <- pptx_images_info(x)
image <- df$node[[1]]
image <- df$node[[1]]
image_rel <- xml_find_first(image, ".//a:blip") |> xml_attr("embed") # // also finds shapes inside groups; maybe only use ".//p:spTree//p:sp"?

# what is collection?
x$slide$collection_get(1)$rel_df()
x$slide$collection_get(1)$relationship()$get_images_path()[image_rel]

# <p:blipFill rotWithShape="1">
#   <a:blip r:embed="rId2" cstate="print"/>
#   <a:srcRect l="8680" r="16918" b="-3"/>
#   <a:stretch/>
# </p:blipFill>
#
xml_get_image_path <- function(x) {
  xml_find_first(x, ".//a:blip") # // also finds shapes inside groups; maybe only use ".//p:spTree//p:sp"?
}


xml_get_images <- function(x) {
  xml_find_all(x, ".//p:pic") # // also finds shapes inside groups; maybe only use ".//p:spTree//p:sp"?
}


# find shapes by text pattern
# x : node or nodeset
# returns: shape nodes
xml_images_find <- function(x, pattern) {
  pics <- x |> xml_get_images()
  texts <- pics |> xml_text()
  ii <- stringr::str_detect(texts, stringr::fixed(pattern))
  shapes[ii]
}


# x : node or nodeset
xml_is_shape <- function(x) {
  all(x |> xml_name() == "sp")
}
