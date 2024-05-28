#' Read PTX file
#' Identical to [officer::read_pptx]. Re-exported for consistent function names.
#' @inheritParams officer::read_pptx
#' @export
pptx_read <- officer::read_pptx


rpptx_print <- getFromNamespace("print.rpptx", "officer")


#' New print method for officer `rpptx` object
#' @inheritParams officer::print.rpptx
#' @param details `[logical]`\cr Show layouts?
#' @export
print.rpptx <- function(x, target = NULL, details = FALSE, ...) {
  if (!is.null(target)) {
    return(rpptx_print(x, target = NULL, ...)) # save object
  }
  cli::cli_h3("rpptx object")
  df <- officer::layout_summary(x)
  n_masters <- df$master |>
    unique() |>
    length()
  n_slides <- length(x)
  n_layouts <- nrow(df)
  active_slide <- x$cursor
  bullets <- c(
    "masters: {.val {n_masters}}",
    "layouts: {.val {n_layouts}}",
    "slides: {.val {n_slides}}",
    "active slide: {.val {active_slide}}"
  )
  names(bullets) <- rep("*", length(bullets))
  cli::cli_bullets(bullets)
  if (details) {
    print(df)
  }
}
