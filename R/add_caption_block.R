#' Add caption block
#' @description A function to insert a caption block in the caption template
#' @keywords internal
#' @export

add_caption_block <- function(i, code) {
  snapshot <- c(
    "",
    paste0("### `", code$lines[i], "`"),
    "",
    "Symbols",
    paste0("\n1. Changed:"),
    paste0("\n2. Added:"),
    paste0("\n3. Deleted:"),
    "",
    "Caption: "
  )
}
