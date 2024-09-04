library(officer)
library(xml2)
devtools::load_all()

# parameters ---------------------------------------------------------------------------------

file_in <- example_file("image_insert")
file_out <- "dev_local/06_add_extra_info_to_shape/presentation_with_extra_info.pptx"

x <- pptx_read(file_in)
# file_open(file_in)
print(x, target = file_out)
# file_open(file_out)


## add namespace -------------------------------------------------------------------------------

# add custom namespace to slide root
# elements prefixes must have a namespace definition
xml_add_ns_to_root <- function(node, prefix = "td", uri = "http://www.talkingdata.de") {
  root <- xml_root(node)
  xml_attr(root, glue::glue("xmlns:{prefix}")) <- uri
  node
}

x <- pptx_read(file_in)
sld <- pptx_slide_get(x, 1)
xml_ns(sld)
# sld |> xml_add_child("td:image")  # error
xml_add_ns_to_root(sld)
xml_ns(sld)
sld |> xml_add_child("td:image")  # now we can add a child with prefix td


## add extLst Info -------------------------------------------------------------------------------

# xml_find_all(shape, "//*[name()='extLst']") # searchj for node name directly

# now we can add a child

x <- pptx_read(file_in)
sld <- pptx_slide_get(x, 1)
xml_add_ns_to_root(sld)
xml_ns(sld)  # new td namespace

df_shapes <- pptx_shapes_info(x)
shape <- df_shapes$node[[1]]
p_ext <- shape |> xml_add_child("p:extLst") |> xml_add_child("p:ext")
p_ext |> xml_add_child("td:info", aa = TRUE, "Hello node")

# find
shape |> xml_find_first(".//p:extLst")
info <- shape |> xml_find_first(".//td:info")
xml_text(info)


# add td:image object -------------------------------------------------------------------------------
# status: PPTX file gets corrupted. cannot read parts of it. Not that easy.
x <- pptx_read(file_in)
# file_open(file_in)
# print(x, target = file_out)
sld <- pptx_slide_get(x, 1) |> xml_add_ns_to_root()

df_shapes <- pptx_shapes_info(x)
shape <- df_shapes$node[[1]]
p_ext <- shape |>
  xml_add_child("p:extLst") |>
  xml_add_child("p:ext", uri="http://talkingdata.de/schema")  # NOTE: will break XML without URI !!
  # xml_add_child("p:ext", uri="http://schemas.openxmlformats.org/officeDocument/2006/customXml")

td_data <- xml_new_root("td:data") |> xml_add_ns_to_root()
td <- td_data |> xml_add_child("td:image")
xml_add_child(td, .value = "timestamp",  Sys.time())
xml_add_child(td, .value = "path", "my/path")
xml_add_child(td, .value = "filename", "image.png")

xml_add_child(p_ext, td_data)
# as.character(shape) |> cat()

print(x, target = file_out)
file_open(file_out)

# info <- list(
#   added = structure()
# )
#
# l <- list(
#   path_local = "local/img/path",
#   added = Sys.time(),
#   frame = list(top= 10, bottom =0)
# )
#
#
# l <- xml2::as_list(td)
# as_xml_document(l)


## add extLst Info -------------------------------------------------------------------------------



# OOXML info:
# - include inside sahpe xml usig proper namespace
# <p:sp xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main"
#       xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
#       xmlns:my="http://schemas.example.com/mycustomxml">
#   <p:nvSpPr>
#     <p:cNvPr id="4" name="Custom Shape"/>
#     <p:cNvSpPr/>
#     <p:nvPr>
#       <p:ph/>
#     </p:nvPr>
#   </p:nvSpPr>
#   <p:spPr>
#     <a:xfrm>
#       <a:off x="0" y="0"/>
#       <a:ext cx="3048000" cy="3048000"/>
#     </a:xfrm>
#     <a:prstGeom prst="rect">
#       <a:avLst/>
#     </a:prstGeom>
#   </p:spPr>
#   <p:txBody>
#     <a:bodyPr/>
#     <a:lstStyle/>
#     <a:p>
#       <a:r>
#         <a:rPr lang="en-US"/>
#         <a:t>Custom Shape with Data</a:t>
#       </a:r>
#     </a:p>
#   </p:txBody>
#   <p:extLst>
#     <p:ext uri="{my-custom-extension-uri}">
#       <my:data>
#         <my:customElement>Custom Value</my:customElement>
#       </my:data>
#     </p:ext>
#   </p:extLst>
# </p:sp>


# <p:sp>
# ...
#   <p:extLst>
#     <p:ext uri="http://talkingdata.de/schema">
#       <td:data>
#         <td:image>
#           <file path="my/path" epoch_ts="1717847331.70234" name="filename.png"/>
#           <frame top="0" left="0" width="3" height="4"></frame>
#           <a:xfrm>
#             <a:off x="0" y="0"/>
#             <a:ext cx="0" cy="0"/>
#             <a:chOff x="0" y="0"/>
#             <a:chExt cx="0" cy="0"/>
#           </a:xfrm>
#         </td:image>
#       </td:data>
#     </p:ext>
#   </p:extLst>
#...
# </p:sp>
