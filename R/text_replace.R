# ____________----
# REPLACE TEXT --------------------------------------------


#' Replace text on slides
#'
#' All occurences of `pattern` on the selected slides are replaced with the text of `replacement`.
#' For the formatting, only the first character of the matched pattern is relevant.
#' The whole replacement will have this format.
#'
#' @param x `[rpptx]`\cr An [officer] object. See [officer::read_pptx()].
#' @param pattern `[character]`\cr Vector with patterns to replace. Regex is not interpreted.
#' @param replacement `[character]`\cr Vector with replacements.
#' @param slide_idx `[numeric]`\cr Index of slides to process. If `NULL` (default), all slides
#'   are processed.
#' @param ... `[key=value]`\cr Comma separated `'pattern'='replacement'` pairs as an alternative to
#'   using the `pattern` and `replacement` args.
#' @param ph_type `[character]`\cr Placeholder types to include (e.g., `"body"`, `"title"`,
#'   `"ftr"`, `"dt"`, `"sldNum"`). If `NULL` (default), all shapes are included.
#'   Non-placeholder shapes are excluded when this is set.
#' @param exclude_ph_type `[character]`\cr Placeholder types to exclude. Applied after
#'   `ph_type` filtering. Non-placeholder shapes are not affected by this filter.
#' @param verbose `[integer]`\cr Level of console output (default `1`).\cr
#'   `0` = silent, `1` = one-line summary, `2` = detailed per-shape breakdown.
#' @param dry_run `[logical]`\cr If `TRUE`, no replacements are performed. The log is still
#'   collected and attached to the returned object (default `FALSE`).
#' @return The (modified) rpptx object with an attribute `"text_replace_log"` containing a
#'   tibble of replacements performed. Use [text_replace_log()] to access it.
#' @export
#' @example inst/ext/examples/example-text-replace.R
text_replace <- function(x, pattern = NULL, replacement = NULL, slide_idx = NULL, ...,
                         ph_type = NULL, exclude_ph_type = NULL,
                         verbose = 1L, dry_run = FALSE) {
  if (!inherits(x, "rpptx")) {
    cli::cli_abort("{.arg x} must be an {.cls rpptx} object.", call = NULL)
  }
  slide_idx <- slide_idx %||% seq_len(x$slide$length())

  dots <- list(...)
  pattern <- c(pattern, names(dots))
  dots_replacement <- dots |> unlist() |> unname() |> as.character()
  replacement <- c(replacement, dots_replacement)
  if (length(pattern) != length(replacement)) {
    cli::cli_abort("Length of {.arg pattern} and {.arg replacement} must match.", call = NULL)
  }

  log <- dplyr::tibble(
    slide_idx = integer(),
    shape_name = character(),
    pattern = character(),
    replacement = character(),
    count = integer()
  )

  for (i in seq_along(slide_idx)) {
    idx <- slide_idx[i]
    shapes <- pptx_shapes_on_slide(x, idx)
    shapes <- filter_shapes_by_ph_type(shapes, ph_type, exclude_ph_type)
    for (si in seq_along(shapes)) {
      shape <- shapes[[si]]
      shape_name <- xml_shape_get_name(shape)
      for (pi in seq_along(pattern)) {
        n <- xml_shape_count_matches(shape, pattern[pi])
        if (n > 0L) {
          log <- dplyr::bind_rows(log, dplyr::tibble(
            slide_idx = idx,
            shape_name = shape_name,
            pattern = pattern[pi],
            replacement = replacement[pi],
            count = n
          ))
          if (!dry_run) {
            xml_shape_text_replace(shape, pattern[pi], replacement[pi])
          }
        }
      }
    }
  }

  attr(x, "text_replace_log") <- log

  if (verbose >= 1L && nrow(log) > 0L) {
    n_total <- sum(log$count)
    n_slides <- length(unique(log$slide_idx))
    prefix <- if (dry_run) "text_replace (dry run)" else "text_replace"
    cli::cli_alert_info("{prefix}: {n_total} replacement{?s} across {n_slides} slide{?s}")
    if (verbose >= 2L) {
      for (r in seq_len(nrow(log))) {
        row <- log[r, ]
        cli::cli_bullets(c(" " = paste0(
          "Slide {row$slide_idx} | {.val {row$shape_name}}: ",
          "{.val {row$pattern}} -> {.val {row$replacement}} ({row$count}x)"
        )))
      }
    }
  } else if (verbose >= 1L && nrow(log) == 0L) {
    cli::cli_alert_info("text_replace: no matches found")
  }

  x
}


#' Get the text replacement log
#'
#' Returns the log tibble from the last [text_replace()] call attached to the object.
#'
#' @param x `[rpptx]`\cr An [officer] object that was processed by [text_replace()].
#' @return A tibble with columns `slide_idx`, `shape_name`, `pattern`, `replacement`, `count`,
#'   or `NULL` if no log is available.
#' @export
text_replace_log <- function(x) {
  attr(x, "text_replace_log")
}


#' Assert expectations on text replacements
#'
#' Checks the replacement log attached to an rpptx object against expected counts.
#' Typically used after [text_replace()] in a pipeline.
#'
#' @param x `[rpptx]`\cr An [officer] object that was processed by [text_replace()].
#' @param pattern `[character(1)]`\cr The pattern to check.
#' @param n `[integer(1)]`\cr Expected exact number of replacements. Mutually exclusive with
#'   `min`/`max`.
#' @param min `[integer(1)]`\cr Minimum expected number of replacements.
#' @param max `[integer(1)]`\cr Maximum expected number of replacements.
#' @param slide_idx `[numeric]`\cr Limit the check to specific slides. If `NULL` (default),
#'   the total across all slides is checked.
#' @return `x` invisibly (for piping).
#' @export
text_replace_expect <- function(x, pattern, n = NULL, min = NULL, max = NULL, slide_idx = NULL) {
  log <- text_replace_log(x)
  if (is.null(log)) {
    cli::cli_abort("No text replacement log found on {.arg x}. Run {.fn text_replace} first.",
                   call = NULL)
  }
  if (!is.null(n) && (!is.null(min) || !is.null(max))) {
    cli::cli_abort("{.arg n} cannot be used together with {.arg min}/{.arg max}.", call = NULL)
  }

  log_filtered <- log[log$pattern == pattern, ]
  if (!is.null(slide_idx)) {
    log_filtered <- log_filtered[log_filtered$slide_idx %in% slide_idx, ]
  }
  actual <- sum(log_filtered$count)

  scope <- if (is.null(slide_idx)) "total" else paste0("slide ", paste(slide_idx, collapse = ", "))

  if (!is.null(n) && actual != n) {
    cli::cli_abort(
      "Expected exactly {n} replacement{?s} of {.val {pattern}} ({scope}), got {actual}.",
      call = NULL
    )
  }
  if (!is.null(min) && actual < min) {
    cli::cli_abort(
      "Expected at least {min} replacement{?s} of {.val {pattern}} ({scope}), got {actual}.",
      call = NULL
    )
  }
  if (!is.null(max) && actual > max) {
    cli::cli_abort(
      "Expected at most {max} replacement{?s} of {.val {pattern}} ({scope}), got {actual}.",
      call = NULL
    )
  }

  invisible(x)
}


# ____________----
# INTERNAL HELPERS --------------------------------------------


# Filter shapes by placeholder type.
# ph_type: include only placeholders of these types (non-placeholders excluded)
# exclude_ph_type: exclude placeholders of these types (non-placeholders kept)
filter_shapes_by_ph_type <- function(shapes, ph_type = NULL, exclude_ph_type = NULL) {
  if (is.null(ph_type) && is.null(exclude_ph_type)) {
    return(shapes)
  }
  if (length(shapes) == 0L) {
    return(shapes)
  }
  types <- vapply(shapes, xml_placeholder_type, character(1))
  keep <- rep(TRUE, length(shapes))
  if (!is.null(ph_type)) {
    keep <- keep & (!is.na(types) & types %in% ph_type)
  }
  if (!is.null(exclude_ph_type)) {
    keep <- keep & (is.na(types) | !types %in% exclude_ph_type)
  }
  shapes[keep]
}


xml_shape_get_name <- function(shape) {
  nvpr <- shape |> xml2::xml_find_first(".//p:cNvPr")
  if (is.na(nvpr)) return(NA_character_)
  xml2::xml_attr(nvpr, "name")
}


xml_shape_count_matches <- function(shape, pattern) {
  all_text <- xml_get_runs(shape) |>
    xml2::xml_text() |>
    paste0(collapse = "")
  str_count_fixed(all_text, pattern)
}


# Repeat a single row `times` at its position in a data frame.
# Used to "expand" the character data frame when the replacement is longer
# than a single character.
df_row_repeat <- function(df, idx = NULL, times = 1) {
  if (is.null(idx) || all(is.na(idx))) {
    return(df)
  }
  if (!is.data.frame(df)) {
    cli::cli_abort("{.arg df} must be a data frame.", call = NULL)
  }
  if (!is.numeric(idx) || length(idx) != 1L || idx < 1L || idx > nrow(df)) {
    cli::cli_abort("{.arg idx} must be a single integer in [1, {nrow(df)}].", call = NULL)
  }
  if (!is.numeric(times) || length(times) != 1L || times < 0L) {
    cli::cli_abort("{.arg times} must be a non-negative integer.", call = NULL)
  }
  ii <- seq_len(nrow(df))
  before <- df[ii < idx, ]
  repeated <- df[rep(idx, each = times), ]
  after <- df[ii > idx, ]
  dplyr::bind_rows(before, repeated, after)
}


# Replace all occurrences of a fixed pattern in a single shape's text.
#
# Strategy: Each run (<a:r>) holds a text fragment. We explode all run texts
# into a one-row-per-character data frame, find match positions on the
# concatenated string, then splice replacement characters in place. Processing
# matches last-to-first keeps earlier row indices stable. Finally we collapse
# characters back per run and write the new text to the XML nodes.
# The replacement inherits the formatting of the first matched character's run.
xml_shape_text_replace <- function(shape, pattern, replacement) {
  text <- original <- run_idx <- NULL
  runs <- xml_get_runs(shape)
  runs_text <- runs |>
    xml2::xml_text() |>
    stats::setNames(paste0("r_", seq_along(runs)))
  text_old <- paste0(runs_text, collapse = "")

  # Locate all non-overlapping matches at once (avoids cascading replacements)
  locs <- str_locate_all_fixed(text_old, pattern)
  if (nrow(locs) == 0L) {
    return(invisible(NULL))
  }

  # Explode run texts into one row per character, keeping track of source run

  df_runs <- dplyr::tibble(run_idx = seq_along(runs_text), text = runs_text)
  df <- df_runs |>
    dplyr::mutate(original = strsplit(text, "")) |>
    tidyr::unnest(original)

  chars_new <- strsplit(replacement, "") |> unlist()
  n_new <- length(chars_new)
  is_empty_replacement <- (replacement == "")
  if (is_empty_replacement) {
    chars_new <- character(0)
    n_new <- 0L
  }

  # Process matches in reverse order so row indices of earlier matches stay valid
  for (m in rev(seq_len(nrow(locs)))) {
    from <- locs[m, "start"]
    to <- locs[m, "end"]
    ii_pattern <- seq(from, to)
    i_first <- ii_pattern[1]
    ii_remove <- ii_pattern[-1]

    # Remove all matched characters except the first (which anchors formatting)
    if (length(ii_remove) > 0) {
      df <- df[-ii_remove, ]
    }

    if (is_empty_replacement) {
      # Deletion: remove the remaining anchor character too
      df <- df[-i_first, ]
    } else {
      # Insert replacement chars at the anchor position (inherits run formatting)
      df <- df_row_repeat(df, idx = i_first, times = n_new)
      ii_replace <- seq(i_first, i_first + n_new - 1)
      df$original[ii_replace] <- chars_new
    }
  }

  # Collapse characters back to per-run strings and write to XML
  .df_runs <- df |> dplyr::summarise(
    .by = run_idx,
    replacement = paste0(original, collapse = "")
  )
  # Runs that lost all characters get NA after the join; replace with ""
  df_all <- df_runs |>
    dplyr::left_join(.df_runs, by = "run_idx") |>
    dplyr::mutate(replacement = tidyr::replace_na(replacement, ""))
  xml2::xml_text(runs) <- df_all$replacement
}
