# all public fields plus values from class object
# either as list or as tibble
get_public_fields <- \(as_tibble = FALSE) {
  # return all public fields as list or tibble
  public_fields <- ls(self)
  public_values <- lapply(public_fields, \(field) self[[field]])
  custom_fields <- public_fields[!vapply(public_values, is.function, logical(1))]
  l <- lapply(custom_fields, \(field) self[[field]]) |> setNames(custom_fields)
  if (!as_tibble) {
    return(l)
  }
  ii <- vapply(l, is.list, logical(1)) # or is_node?
  l[ii] <- lapply(l[ii], \(x) list(x)) # wrap xml nodes in list to put into tibble
  tibble::as_tibble_row(l)
}


cl <- cli::col_magenta # short form for color



# _____________ ----
# <BaseShape> --------------------------------------------------------
#
# OOXML shape class docs: https://learn.microsoft.com/de-de/dotnet/api/documentformat.openxml.vml.shape?view=openxml-3.0.1
# We will not reproduce the whole class, just the things which are needed for your purposes.
# see python-pptx shape class for orientation: https://github.com/scanny/python-pptx/blob/master/src/pptx/shapes/base.py
#
# We will currently ignore shape hierarchies and group shapes. For our purpose it suffices,
# to treat each shapes as single entities.

# http://officeopenxml.com/drwShape.php:
# There are four basic components of a shape, corresponding to the four child elements:
#
# <nvSpPr> - non-visual shape properties. See Shapes - Non-Visual Properties.
# <spPr> - shape properties. See Shapes - Shape Properties.
# <style> - shape styles. See Shapes - Styles.
# <txBody> - text within the shape. See Shapes - Text.
#
BaseShape <- R6::R6Class(
  "BaseShape",
  public = list(
    xml = NA,
    initialize = \(xml_node) {
      self$xml <- xml_node
    },
    print = \(...) {
      cli::cli_alert_info(
        "<BaseShape id:{cl(self$id)} name:{cl(self$name)} type:{cl(self$type)} is_placeholder:{cl(self$is_placeholder)} hidden:{cl(self$hidden)}
        left:{cl(self$left)} top:{cl(self$top)} width:{cl(self$width)} height:{cl(self$height)} rotation:{cl(self$rotation)}
        has_textframe:{cl(self$has_textframe)} text:{cl(self$text)}
      >"
      )
    },
    get_public_fields = get_public_fields,
    print_xml = \(full = TRUE) {
      if (full) {
        self$xml |>
          paste() |>
          cat()
      } else {
        xml2::xml_structure(self$xml)
      }
    },
    remove = \() {
      xml2::xml_remove(self$xml)
    }
  ),
  active = list(
    id = \(value) {
      node <- xml2::xml_child(self$xml, "p:nvSpPr/p:cNvPr")
      if (missing(value)) {
        xml2::xml_attr(node, "id")
      } else {
        cli::cli_abort("{.val id} is a read-only property.", call = NULL)
      }
    },
    name = \(value) {
      node <- xml2::xml_child(self$xml, "p:nvSpPr/p:cNvPr")
      if (missing(value)) {
        xml2::xml_attr(node, "name")
      } else {
        xml2::xml_set_attr(node, "name", value)
      }
    },
    text = \(value) {
      if (missing(value)) {
        self$xml |> xml2::xml_text()
      } else {
        xml2::xml_text(self$xml) <- value
      }
    },
    hidden = \(value) {
      node <- xml2::xml_child(self$xml, "p:nvSpPr/p:cNvPr")
      if (missing(value)) {
        hidden <- xml2::xml_attr(node, "hidden")
        ifelse(is.na(hidden), FALSE, as.logical(as.integer(hidden)))
      } else {
        xml2::xml_set_attr(node, "hidden", as.integer(value))
      }
    },
    left = \(value) {
      node <- xml_child(self$xml, "p:spPr/a:xfrm/a:off")
      if (missing(value)) {
        xml2::xml_attr(node, "x") |> as.integer()
      } else {
        xml2::xml_set_attr(node, "x", as_integer(value))
      }
    },
    top = \(value) {
      node <- xml_child(self$xml, "p:spPr/a:xfrm/a:off")
      if (missing(value)) {
        xml2::xml_attr(node, "y") |> as.integer()
      } else {
        xml2::xml_set_attr(node, "y", as_integer(value))
      }
    },
    width = \(value) {
      node <- xml2::xml_child(self$xml, "p:spPr/a:xfrm/a:ext")
      if (missing(value)) {
        xml2::xml_attr(node, "cx") |> as.integer()
      } else {
        xml2::xml_set_attr(node, "cx", as_integer(value))
      }
    },
    height = \(value) {
      node <- xml2::xml_child(self$xml, "p:spPr/a:xfrm/a:ext")
      if (missing(value)) {
        xml2::xml_attr(node, "cy") |> as.integer()
      } else {
        if (is.na(node)) {
          cli::cli_abort(
            c("Shape has no position or size info.",
              "i" = "Is this a placeholder which has not been changed?"
            )
          )
        }
        xml2::xml_set_attr(node, "cy", as_integer(value))
      }
    },
    rotation = \(value) {
      # unit: degrees
      node <- xml2::xml_child(self$xml, "p:spPr/a:xfrm")
      if (missing(value)) {
        xml2::xml_attr(node, "rot") |>
          as.integer() |>
          rotation_to_degree()
      } else {
        xml2::xml_set_attr(node, "rot", degree_to_rotation(value))
      }
    },
    is_placeholder = \(value) {
      if (missing(value)) {
        !xml2::xml_child(self$xml, "p:nvSpPr/p:nvPr/p:ph") |> is.na() # has p:ph element?
      } else {
        cli::cli_abort("{.val is_placeholder} is a read-only property.", call = NULL)
      }
    },
    type = \(value) {
      if (missing(value)) {
        res <- xml2::xml_child(self$xml, "p:spPr/a:prstGeom") |> xml2::xml_attr("prst") # Preset Geometry
        is_custom_geom <- !xml2::xml_child(self$xml, "p:spPr/a:custGeom") |> is.na() # has p:ph element?
        if (is.na(res) && is_custom_geom) {
          res <- "<CustomGeom>"
        }
        res
      } else {
        cli::cli_abort("{.val type} is a read-only property.", call = NULL)
      }
    },
    has_textframe = \(value) {
      if (missing(value)) {
        !xml2::xml_child(self$xml, "p:txBody") |> is.na() # has textframe?
      } else {
        cli::cli_abort("{.val has_textframe} is a read-only property.", call = NULL)
      }
    }
  ),
  private = list()
)


xml_text <-
  '
<p:sld xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:p="http://schemas.openxmlformats.org/presentationml/2006/main">
<p:sp>
    <p:nvSpPr>
      <p:cNvPr id="5" name="Textfeld 4">
        <a:extLst>
          <a:ext uri="{FF2B5EF4-FFF2-40B4-BE49-F238E27FC236}">
            <a16:creationId xmlns:a16="http://schemas.microsoft.com/office/drawing/2014/main" id="{9FA7E404-0E53-4735-00A1-CACBC4F7FC37}"/>
          </a:ext>
        </a:extLst>
      </p:cNvPr>
      <p:cNvSpPr txBox="1"/>
      <p:nvPr/>
    </p:nvSpPr>
    <p:spPr>
      <a:xfrm>
        <a:off x="107504" y="116632"/>
        <a:ext cx="720080" cy="369332"/>
      </a:xfrm>
      <a:prstGeom prst="rect">
        <a:avLst/>
      </a:prstGeom>
      <a:noFill/>
    </p:spPr>
    <p:txBody>
      <a:bodyPr wrap="square" rtlCol="0">
        <a:spAutoFit/>
      </a:bodyPr>
      <a:lstStyle/>
      <a:p>
        <a:r>
          <a:rPr lang="de-DE" dirty="0" err="1"/>
          <a:t>hello</a:t>
        </a:r>
        <a:endParaRPr lang="de-DE" dirty="0"/>
      </a:p>
    </p:txBody>
  </p:sp>
</p:sld>
'
.node <- read_xml(xml_text) |> xml2::xml_find_first("//p:sp")
.shp <- BaseShape$new(.node)

# _____________ ----
# <ShapeTree> --------------------------------------------------------

#
# contains all shapetree elements on slide: shapes, picture, graphicframe
#
ShapeTree <- R6::R6Class(
  "ShapeTree",
  public = list(
    xml = NA,
    elements = list(),
    initialize = \(xml_node) {
      assert_node(xml_node, "xml_node")
      if (is_shapetree(xml_node)) {
        self$xml <- xml_node
      } else {
        shp_tree <- xml2::xml_child(xml_node, "*/p:spTree")
        assert_shapetree(shp_tree, "xml_node")
        self$xml <- shp_tree
      }
    },
    print = \(...) {
      cli::cli_alert_info(
        "<ShapeTree n:{self$n}>"
      )
    },
    get_public_fields = get_public_fields,
    print_xml = \(full = TRUE) {
      if (full) {
        self$xml |>
          paste() |>
          cat()
      } else {
        xml2::xml_structure(self$xml)
      }
    },
    remove = \() {
      xml2::xml_remove(self$xml)
    }
  ),
  active = list(
    n = \() {
      length(self$elements)
    }
  ),
  private = list()
)
