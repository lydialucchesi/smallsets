#' Add caption block
#' @description A function to insert a caption block in the caption template
#' @keywords internal
#' @export

add_caption_block <- function(i, col1, col2, col3, code) {
  snapshot <- c(
    "",
    paste0("### `", code$lines[i], "`"),
    "",
    "Symbols",
    paste0("\n1. Changed (", col1, "):"),
    paste0("\n2. Added (", col2, "):"),
    paste0("\n3. Deleted (", col3, "):"),
    "",
    "Caption: "
  )
}
