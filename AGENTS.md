# Agent Notes

This repository is the `officer.pptx` R package, an add-on to the `officer`
package for working with PowerPoint (PPTX) files. These notes capture
project-specific conventions and user preferences for future coding agents.

## Working Style

- Prefer small, focused changes over broad rewrites.
- Build context from the codebase before changing behavior.
- Add tests incrementally, in small batches.
- Follow the Tidyverse style guide for R code: https://style.tidyverse.org/.
- Run `styler` after code changes.
- Do not add backward-compatibility code unless there is a concrete need.
- Do not commit unless explicitly asked. If asked for a commit message, provide
  only the message unless a commit was requested.
- If rewriting commit dates, update both author and committer dates.
- Preserve unrelated user changes in the worktree.
- **After completing a feature (i.e., after a commit)**, proactively ask the user
  if there are general learnings that should be added to AGENTS.md.

## Development Change Discipline

- When committing development changes, always bump `DESCRIPTION` `Version` and
  add the relevant entry to `NEWS.md`.
- For the current development version, keep one `NEWS.md` header at the release
  version level, e.g. `# officer.pptx 0.1.1`; do not create separate headers for
  each development patch version such as `0.1.1.9013`.
- If an issue number is known, reference it in both the `NEWS.md` entry and the
  commit message.

## R Package Context

- This is a testthat 3 R package.
- Important package files include `DESCRIPTION`, `NAMESPACE`, `R/`,
  `tests/testthat/`, `vignettes/`, `README.Rmd`, and `README.md`.
- `README.md` is generated from `README.Rmd`. For simple text changes, update
  both files manually instead of rendering.
- Package articles are source files under `vignettes/`. Generated `doc/` output
  is ignored by git and should not be edited directly.
- For user-facing console output, prefer the `cli` package with colored output
  and structured elements over plain `print()`/`cat()` output.
- For long-running user-facing functions, provide intermediate progress or
  status messages with the `cli` package so users know what is happening.
- Use `call = NULL` in `cli::cli_abort()` calls for cleaner error messages.

## Dependencies

- The package depends on `officer` (>= 0.6.8) and imports common tidyverse
  packages (`dplyr`, `tidyr`, `purrr`, `stringr`, `rlang`), plus `xml2` for XML
  manipulation, `cli` for user output, and `checkmate` for argument validation.
- Suggested packages include `testthat`, `processx`, `pdftools`, `doconv`,
  `magick`, and `quarto`.
- Some functions require LibreOffice to be installed (e.g., `slide_to_png`).

## Officer Package Integration

- **Always leverage internal officer functions** instead of reimplementing
  functionality. Many helper functions exist in officer that handle placeholder
  resolution, location fortification, XML generation, etc.
- Use `officer:::` to access non-exported functions when needed (e.g.,
  `officer:::fortify_location`, `officer:::get_slide_layout`,
  `officer:::resolve_location`).
- Officer source code is available locally at: `/Users/markheckmann/Workspace/mh/officer`
- Officer GitHub repository: https://github.com/davidgohel/officer
- Before implementing new functionality, search the officer codebase for existing
  solutions or helper functions that can be reused.
- Useful internal officer functions:
  - `officer:::resolve_location()` - converts strings/numbers to `ph_location_*()`
  - `officer:::fortify_location()` - resolves locations to coordinates
  - `officer:::get_slide_layout()` - gets layout info for a slide
  - `officer:::is_ph_location()` - checks if object is a location
  - `layout_properties()` - returns all placeholders for a layout

## Coding Conventions

- **No checkmate**: Use simple `if`-checks with `cli::cli_abort()` for input
  validation instead of checkmate assertions.
- **Parameter naming**: Keep consistent with `img()` and officer conventions:
  - Use `background` (not `fill`) for background colors
  - Use `line` with `sp_line()` for border styling
- **Examples**: Put roxygen examples in separate files under `inst/ext/examples/`
  and reference them with `@example inst/ext/examples/example-<function>.R`.

## Testing Conventions

- Tests live in `tests/testthat/`.
- Test data files are in `tests/testthat/testdata/` and `tests/testthat/testimages/`.
- Snapshot tests are stored in `tests/testthat/_snaps/`.
- When running a single source test file for recent code changes, load the source
  package first:
  - `devtools::load_all()`
  - `testthat::test_file("tests/testthat/<file>.R")`
- Calling `testthat::test_file()` directly may use an installed package instead
  of the working tree.

## Useful Test Commands

Run all tests:

```r
devtools::test()
```

Run a single test file:

```r
devtools::load_all()
testthat::test_file("tests/testthat/test-frame.R")
```

Run R CMD check:

```r
devtools::check()
```

Run coverage:

```r
cov <- covr::package_coverage(type = "tests", quiet = TRUE)
covr::percent_coverage(cov)
```

## Documentation Conventions

- Keep README concise.
- Put longer contributor/developer documentation in `vignettes/` so pkgdown can
  build it as an article.
- Do not place source documentation under `doc/`; pkgdown/build output goes
  there and the directory is ignored.
- Use roxygen2 with markdown enabled (`Roxygen: list(markdown = TRUE)`).
- For `@param x` documentation on rpptx objects, use fully qualified links:
  `[rpptx]\cr An [officer::officer] object. See [officer::read_pptx()].`

## Vignettes

- Vignettes use quarto. Render with:
  ```bash
  /Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto render vignettes/<file>.qmd
  ```
- If new functions are used in the vignette, install the package first:
  `devtools::install()`.
- Helper functions (like `ph_frame()`) should be defined in visible code blocks,
  not hidden setup chunks, so readers understand what is happening.

## Key Functions and Architecture

- `img()`: Position images inside placeholders (successor to `external_img()`).
- `slide_to_png()`, `slide_preview()`: Convert slides to PNG or plot them.
- `text_replace()`: Replace text patterns while preserving formatting.
- `shape_hide()`, `shape_unhide()`: Toggle shape visibility.
- `image_insert()`: Insert images at shape positions.
- `clone_slide()`: Clone existing slides.
- `frame` class: Represents bounding boxes for shapes/placeholders.

## Local Development Folders

- `dev_local/` and `dev_local*/` are ignored and used for local experiments.
- Dev vignettes live in `dev_local/vignettes/` and are not built by pkgdown.
- Do not commit contents from these folders.

## GitHub / CI

- The remote is hosted on GitHub at https://github.com/markheckmann/officer.pptx.
- Use `gh` for issues, PRs, and CI interaction.
- CI workflows: R-CMD-check (multi-platform), test-coverage (Codecov), pkgdown
  (GitHub Pages).
- pkgdown requires quarto; dev vignettes are excluded from the site build.
- Snapshot tests are skipped on CRAN.

## Project-Specific Behavior To Preserve

- `text_replace()` auto-sorts patterns longest-first to prevent substring
  collisions.
- The replacement log is stored as an attribute on the rpptx object, not a side
  effect.
- `xml_shape_text_replace()` processes matches in reverse order to keep row
  indices stable.
- `img()` auto-detects image dimensions for supported formats (jpeg, png, gif,
  tiff, svg, emf, wmf, pdf).

## Commit Message Style

- Use concise conventional-style headers where appropriate, followed by a few
  lines explaining what changed and why.
- Do not return only the header unless explicitly asked for a one-line commit
  message.
- Examples of suitable headers:
  - `test: add coverage for frame helpers (#12)`
  - `docs: update README examples`
  - `feat: add clone_slide function`
  - `fix: correct cli_abort call parameter`
