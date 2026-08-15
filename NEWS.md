# officer.pptx 0.1.1

* `text_replace()` now auto-sorts patterns longest-first to prevent substring
  collisions, validates that all `...` args are named, and uses faster log
  accumulation internally.
* `text_replace()` validates `slide_idx` bounds and returns early with an empty
  log when no patterns are supplied.
* `text_replace_log()` and `text_replace_expect()` gain roxygen examples.
* `text_replace()` gains `verbose`, `dry_run` arguments and a replacement log.
  Use `text_replace_log()` to inspect what was replaced (slide, shape, pattern, count).
  Use `text_replace_expect()` to assert expected replacement counts, optionally per slide (#3).
* `ph_visualize()` to highlight placeholders on a slide with borders and/or background colors.
  Useful for debugging placeholder positions and designing slide content (#13).
* `img()` as a successor to `external_img()` to position an image inside a placeholder. It is easier to use with 
  more options (#12). File formats that support size-detection: jpeg, png, gif, tiff, rsvg, emf, svg, pdf, wmf.
* remove `add_slide()` patch as now integrated into `{officer}`.
* remove `phs_with()`. Ported to `{officer}` as of version `0.6.8`.
* `slide_preview` to plot a slide, `slide_to_png` to save a slide to file. Experimental version `slide_preview_2` 
  leveraging the `doconv` package, but with some problems (#5).
* `add_slide`: gains dynamic dots (passed on to `phs_with`) and arg `.phs_with` to create slide and fill placeholders 
  in one step.
* `phs_with` gains dynamic dots.
* `xml_add_image_at_shape`: fix vectorization. Only works for non placeholder shapes.

# officer.pptx 0.1.0

* `phs_with`: simplified adding of objects to a slide using key-value pair syntax.
* WIP: `pptx_slide_draw`: draw wireframe-like image of elements (shapes, placeholders, images) on a slide.
* `pptx_image_insert_at_shape_temp`, `pptx_shapes_hide_temp`, and `pptx_shapes_unhide_temp` as drafts
used in production. Also several helper functions.
* new tests
* `frame` class to represent a bounding box (used for shapes etc.) and associated functions `frame_*`
* `example_file`, `test_file`, and `test_image` to find example and test files.
* added several xml and pptx helper functions incl. tests
* `pptx_text_replace`: replace text patterns in powerpoint slides. Formatting is respected.
