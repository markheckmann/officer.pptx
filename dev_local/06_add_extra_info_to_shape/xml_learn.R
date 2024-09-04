library(xml2)
d <- xml_new_root("sld",
                  "xmlns" = "http://www.opengis.net/sld",
                  "xmlns:ogc" = "http://www.opengis.net/ogc",
                  "xmlns:se" = "http://www.opengis.net//se",
                  version = "1.1.0"
) %>%
  xml_add_child("layer") %>%
  xml_add_child("se:Name", "My Layer") %>%
  xml_root()
xml_attr(d, "xmlns:td") <- "http://www.talkingdata.de"
xml_ns(d)
d |> xml_add_child("td:image", "Image info")
d
library(xml2)
d <- xml_new_root("sld",
                  "xmlns" = "http://www.opengis.net/sld",
                  "xmlns:ogc" = "http://www.opengis.net/ogc",
                  "xmlns:se" = "http://www.opengis.net//se",
                  version = "1.1.0"
) %>%
  xml_add_child("layer") %>%
  xml_add_child("se:Name", "My Layer") %>%
  xml_root()
xml_ns(d)

xml_set_namespace(d, prefix = "td", "http://abc.com")


d <- '<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE document SYSTEM "CommonMark.dtd">
<document xmlns="http://commonmark.org/xml/1.0">
  <paragraph>
    <text xml:space="preserve">test</text>
  </paragraph>
</document>'

library(xml2)
dx <- xml2::read_xml(d)
xml_add_child(dx, "code_block", "# test\n")
dx <- read_xml(as.character(dx))

d1 <- xml_find_all(dx, "//d1:code_block")
d1
#> {xml_nodeset (1)}
#> [1] <code_block># test\n</code_block>



x <- read_xml('
<catalog xmlns="http://www.talkingdata.de">
  <cd>
    <artist>Sufjan Stevens</artist>
    <title>Illinois</title>
    <src>http://www.sufjan.com/</src>
  </cd>
  <cd>
    <artist>Stoat</artist>
    <title>Future come and get me</title>
    <src>http://www.stoatmusic.com/</src>
  </cd>
  <cd>
    <artist>The White Stripes</artist>
    <title>Get behind me satan</title>
    <src>http://www.whitestripes.com/</src>
  </cd>
</catalog>')
xml_ns(x)
xml_find_all(x, "//d1:cd")
