#' Read PPTX file
#' Identical to [officer::read_pptx]. Re-exported for consistent function names.
#' @inheritParams officer::read_pptx
#' @export
pptx_read <- officer::read_pptx

rpptx_print <- getFromNamespace("print.rpptx", "officer")

stop_if_not_rpptx <- getFromNamespace("stop_if_not_rpptx", "officer")

stop_if_not_class <- getFromNamespace("stop_if_not_class", "officer")

stop_if_not_in_slide_range <- getFromNamespace("stop_if_not_in_slide_range", "officer")

get_layout <- getFromNamespace("get_layout", "officer")
convin <- getFromNamespace("convin", "officer")
is.color <- getFromNamespace("is.color", "officer")


#' New print method for officer `rpptx` object
#' @inheritParams officer::print.rpptx
#' @param details `[logical]`\cr Show layouts?
#' @noRd
print.rpptx <- function(x, target = NULL, details = FALSE, ...) {
  if (!is.null(target)) {
    return(rpptx_print(x, target = target, ...)) # save object
  }
  cli::cli_h3("rpptx object")
  df <- officer::layout_summary(x)
  n_layouts_per_master <- sapply(split(df, df$master), nrow)
  n_layouts <- glue::glue("{n_layouts_per_master} [{names(n_layouts_per_master)}]")
  n_slides <- length(x)
  active_slide <- x$cursor
  bullets <- c(
    "slides: {.val {n_slides}}",
    "active slide: {.val {active_slide}}",
    "layouts: {.val {n_layouts}}"
  )
  names(bullets) <- rep("*", length(bullets))
  cli::cli_bullets(bullets)
  if (details) {
    print(df)
  }
}


cm_to_inches <- function(x) {
  x / 2.54
}

mm_to_inches <- function(x) {
  x / 25.4
}

convin <- function(unit, x) {
  unit <- match.arg(unit, choices = c("in", "cm", "mm"), several.ok = FALSE)
  if (!identical("in", unit)) {
    x <- do.call(paste0(unit, "_to_inches"), list(x = x))
  }
  x
}
