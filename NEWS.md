# officer.pptx 0.1.1

* `shape_add()` and `shape_update()` now write replacement text into DrawingML
  `<a:t>` nodes so text inside created or updated shapes is rendered by
  PowerPoint/LibreOffice.
* `shape_update()` now marks changed text runs as dirty so PowerPoint/LibreOffice
  renderers refresh the visible text in updated template shapes.
* The shape-selection vignette examples now use the packaged `shapes.pptx`
  template instead of constructing example slides from scratch. The thumbnail
  regeneration script is tracked alongside the vignette thumbnail assets.
* `shape_update()` now adds rectangular geometry before applying fill or line
  styling to placeholder text boxes that do not have direct geometry.
* The shape-selection vignette is now structured as a more user-friendly guide
  with a quick workflow, clearer matching examples, grey-bordered thumbnails,
  and a less technical title.
* The shape-selection vignette now calls out the experimental status of the
  `shape_*()` API.
* `shape_*()` function references now show an experimental lifecycle badge.
* `shape_select()` selections can now be piped directly into `shape_update()`,
  which returns the source `rpptx` object and lets shape updates stay inside a
  presentation pipeline.
* `shape_update()` now rejects non-finite or out-of-range transform values before
  modifying slide XML, preventing invalid `NA` coordinate or rotation attributes.
* `shape_add()` creates regular non-placeholder PowerPoint shapes with direct
  coordinates, vectorized styling, preset geometry, and immediate compatibility
  with `shape_select()` and `shape_update()` (#16).
* `shape_update()` modifies shapes returned by `shape_select()`, including
  ordinary non-placeholder shapes, with support for text, solid background,
  line, Selection Pane name, alt text description, and visibility (#15).
* `shape_select()` selections now carry internal source metadata so mutation
  functions can reject stale selections or selections from another presentation (#15).
* `shape_select()` selects top-level slide shapes by visible text and metadata,
  returning shape coordinates and XML nodes for downstream templating workflows (#14).
* The `shape_select()` vignette documents the current selector features, matching
  behavior, result columns, and first-version limitations (#14).
* The `img()` vignette now uses committed static thumbnails so the pkgdown site
  always shows images and no longer depends on live LibreOffice conversion for
  those figures.
* The `img()` vignette now treats slide thumbnail generation as optional during
  pkgdown builds, so site generation does not fail if `doconv` or LibreOffice
  cannot convert a presentation on CI.
* Examples and vignettes that require LibreOffice now skip the conversion step
  when `soffice` is unavailable, so R CMD check can run on standard CI runners.
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
