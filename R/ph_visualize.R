# Placeholder visualization ------------------------------------------------

#' Visualize placeholders on a slide
#'
#' Adds visual indicators (border and/or background) to placeholders on the current
#' slide to help understand the layout. Useful for debugging placeholder
#' positions and designing slide content.
#'
#' @param x `[rpptx]`\cr An rpptx object. See [officer::read_pptx()].
#' @param ph Placeholder specification. Can be:
#'
#'   - `NULL` (default): all placeholders on the slide
#'   - Type string: `"body"`, `"title"`, `"ftr"`, `"dt"`, `"sldNum"`, etc.
#'   - Type with index: `"body[1]"`, `"body[2]"`
#'   - Label string: `"Content Placeholder 2"`
#'   - Special: `"left"`, `"right"`
#'   - A `ph_location_*()` object: [officer::ph_location_type()],
#'     [officer::ph_location_label()], [officer::ph_location_id()]
#'   - Character vector for multiple: `c("body", "title")`
#'
#' @param slide_idx `[integer]`\cr Slide index. `NULL` (default) uses the current slide.
#' @param background Background color (e.g., `"#BBDEFB"`, `"lightblue"`). Default `NULL` (transparent).
#' @param line Border specification via [officer::sp_line()]. Default is a dashed
#'   blue line. Use `NULL` for no border.
#' @param label `[logical]`\cr Show placeholder label/type as text overlay?
#'   Default `FALSE`.
#'
#' @return The rpptx object with visualization overlays added.
#' @export
#' @example inst/ext/examples/example-ph-visualize.R
ph_visualize <- function(x,
                         ph = NULL,
                         slide_idx = NULL,
                         background = NULL,
                         line = sp_line(color = "#1565C0", lwd = 2, lty = "dash"),
                         label = FALSE) {
  stop_if_not_rpptx(x)
  if (!is.null(slide_idx)) x <- on_slide(x, slide_idx)

  # Check for no visualization options
  if (is.null(background) && is.null(line) && !label) {
    cli::cli_warn("No visualization options specified. Set {.arg background}, {.arg line}, or {.arg label}.")
    return(x)
  }

  # Get placeholder locations
  locations <- get_ph_locations(x, ph)
  if (length(locations) == 0) {
    cli::cli_warn("No matching placeholders found.")
    return(x)
  }

  # Add overlays

  transp_png <- system.file("ext/images/transparent.png", package = "officer.pptx")
  for (loc in locations) {
    if (!is.null(background) || !is.null(line)) {
      x <- ph_with(x,
        value = img(transp_png, fit = "fill", background = background, line = line %||% sp_line()),
        location = ph_location(left = loc$left, top = loc$top, width = loc$width, height = loc$height)
      )
    }
    if (label) {
      x <- ph_with(x,
        value = fpar(ftext(sprintf("%s\n[%s]", loc$ph_label, loc$type), fp_text(font.size = 9, color = "#424242"))),
        location = ph_location(left = loc$left + 0.05, top = loc$top + 0.05, width = loc$width - 0.1, height = 0.5)
      )
    }
  }
  x
}


# Get fortified locations for placeholder specs
# TODO: Consider proposing this as an internal officer function, e.g. get_ph_locations()
# It would be useful to have a single function that returns all placeholder locations
# for the current slide, or resolves a vector of ph specs to fortified locations.
get_ph_locations <- function(x, ph) {
  if (is.null(ph)) {
    # All placeholders from layout
    layout_info <- officer:::get_slide_layout(x, x$cursor)
    props <- layout_properties(x, layout = layout_info$layout_name, master = layout_info$master_name)
    return(lapply(seq_len(nrow(props)), function(i) {
      list(left = props$offx[i], top = props$offy[i], width = props$cx[i],
           height = props$cy[i], ph_label = props$ph_label[i], type = props$type[i])
    }))
  }

  # Convert to list of specs
  ph_specs <- if (is.character(ph)) as.list(ph) else if (officer:::is_ph_location(ph)) list(ph) else ph

  # Resolve each spec
  locations <- lapply(ph_specs, function(p) {
    if (is.character(p) && p == "fullsize") {
      cli::cli_warn("{.val fullsize} is not a layout placeholder, skipping.")
      return(NULL)
    }
    tryCatch({
      officer:::fortify_location(officer:::resolve_location(p), doc = x)
    }, error = function(e) NULL)
  })

  # Remove NULLs and duplicates
  locations <- Filter(Negate(is.null), locations)
  seen <- character()
  Filter(function(loc) {
    if (loc$ph_label %in% seen) FALSE else { seen <<- c(seen, loc$ph_label); TRUE }
  }, locations)
}
