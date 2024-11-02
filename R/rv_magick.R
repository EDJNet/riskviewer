#' Converts output to an image object in standard formats.
#'
#' @param plot A plot, typically generated with e.g. `rv_create_arena()`
#' @param quality One of either "low" (output 1024px width), "medium" (output 1512px width), or "high" (output 2048px width). Defaults to "low".
#' @param bg Background colour.
#' @param stack Logical, defaults to FALSE. If TRUE, graphs are placed left-to-right, if FALSE top-to-bottom.
#'
#' @return
#' @export
#'
#' @examples
#'
#' plot <- rv_create_arena(ratio = 0.7)
#' rv_img(plot, quality = "medium")
#'
rv_img <- function(plot,
                   quality = "low",
                   bg = "white",
                   stack = FALSE) {
  if (quality == "low") {
    graphs_img <- magick::image_graph(
      res = 96,
      pointsize = 12,
      width = 1251,
      height = 1251,
      bg = bg
    )
    if (is.element("list", class(plot))) {
      purrr::walk(.x = plot, .f = print)
    } else {
      print(plot)
    }
    invisible(dev.off())
  } else if (quality == "medium") {
    graphs_img <- magick::image_graph(
      res = 150,
      pointsize = 12,
      width = 1859,
      height = 1859,
      bg = bg
    )

    if (is.element("list", class(plot))) {
      purrr::walk(.x = plot, .f = print)
    } else {
      print(plot)
    }
    invisible(dev.off())
  } else if (quality == "high") {
    graphs_img <- magick::image_graph(
      res = 300,
      pointsize = 12,
      width = 2523,
      height = 2523,
      bg = bg
    )

    if (is.element("list", class(plot))) {
      purrr::walk(.x = plot, .f = print)
    } else {
      print(plot)
    }
    invisible(dev.off())
  }

  if (is.element("list", class(plot))) {
    graphs_img %>%
      magick::image_trim() %>%
      magick::image_border(color = "white") %>%
      magick::image_append(stack = stack)
  } else {
    graphs_img %>%
      magick::image_trim() %>%
      magick::image_border(color = "white")
  }
}
