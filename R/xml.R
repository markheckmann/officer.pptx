
xml_get_shapes <- function(x) {
  xml2::xml_find_all(x, ".//p:sp") # // also finds shapes inside groups; maybe only use ".//p:spTree//p:sp"?
}


xml_get_runs <- function(x) {
  xml2::xml_find_all(x, ".//a:p/a:r")
}
