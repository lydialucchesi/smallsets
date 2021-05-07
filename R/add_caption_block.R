#' Add caption block
#' @description The function inserts a snapshot section into the caption
#'   template.
#' @keywords internal

add_caption_block <- function(i, code) {
  snapshot <- c("",
                paste0("### `", code$lines[i], "`"),
                "",
                "Caption: ")
}
