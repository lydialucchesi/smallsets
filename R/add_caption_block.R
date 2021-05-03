#' Add caption block
#' @description A function to insert a caption block in the caption template
#' @keywords internal
#' @export

add_caption_block <- function(i, code) {
  snapshot <- c(
    "",
    paste0("### `", code$lines[i], "`"),
    "",
    "Caption: "
  )
}
