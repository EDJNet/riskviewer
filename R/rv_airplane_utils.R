#' Create a dataframe with seats location and names (used internally)
#'
#' @param rows Numeric, defaults to 33
#' @param seats_per_row Numeric, defaults to 6. Untested with anything different from 6.
#'
#' @return
#' @export
#'
#' @examples
rv_create_airplane_seats <- function(rows = 33,
                                     seats_per_row = 6) {
  column_position <- tibble::tibble(column = LETTERS[1:seats_per_row],
                                    position = c(2:sum(seats_per_row/2,1),
                                                 sum(seats_per_row/2,3):sum(seats_per_row/2,3,2)))
  
 tidyr::expand_grid(row = -1:-rows,
                              column = LETTERS[1:seats_per_row]) %>%
    dplyr::left_join(column_position, by = "column") %>%
    dplyr::mutate(seat_name = stringr::str_c(
      stringr::str_pad(string = paste0(column, row),
                       width = 3, side = "both"),
      "\n"
    ))
}


#' Create a ggplot based on seats typically generated with `rv_create_airplane_seats()`
#'
#' @param seats 
#' @param fill 
#' @param na_colour 
#' @param rows 
#' @param title 
#' @param font_family 
#' @param legend_position 
#'
#' @return
#' @export
#'
#' @examples
rv_create_airplane_graph_compact <- function(seats, 
                                             rows = 33,
                                             fill = c("coral1", "darkcyan", "darkgoldenrod1", "darkseagreen1", "lightskyblue1"),
                                             na_colour = "grey80",
                                             levels = NULL,
                                             title = NULL, 
                                             font_family = "sans",
                                             legend_position = "none") {
  if (is.vector(levels)==FALSE) {
    seats <- seats %>% 
      dplyr::mutate(risk_places = factor(x = risk_places,
                                         levels = levels))
    levels <- levels(seats$risk_places)[is.na(levels(seats$risk_places)==FALSE)]
  }
  

  
  airplane_gg <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = seats,
      mapping = ggplot2::aes(x = position,
                             y = row,
                             colour = risk_places
      ), shape = 15, size = 2) +
    ggplot2::scale_colour_manual(values = fill, na.value = na_colour, na.translate = TRUE, drop = FALSE, breaks = levels) +
    ggplot2::geom_segment(mapping = ggplot2::aes(x = 1, y = -rows-2, xend = 1, yend = -0.9), size = 2, color = "black", lineend = "round") +
    ggplot2::geom_segment(mapping = ggplot2::aes(x = 9, y = -rows-2, xend = 9, yend = -0.9), size = 2, color = "black", lineend = "round") +
    ggplot2::geom_curve(mapping = ggplot2::aes(x = 1, y = -1, xend = 9, yend =-1), size = 2, color = "black", curvature = -1, lineend = "round", ncp = 20) +
    ggplot2::scale_y_continuous(name = "",
                                limits = c(-rows-2, 3.5),
                                breaks = -1:-rows,
                                labels = stringr::str_pad(string = 1:rows, width = 2),
                                expand = c(0, 0)) +
    ggplot2::scale_x_continuous(name = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(text = ggplot2::element_text(size = 12,
                                                family = font_family),
                   panel.grid = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   legend.position = legend_position,
                   legend.title = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(0, 0, 0, 0, "lines")) 
  
  if (is.null(title)==FALSE) {
    airplane_gg <- airplane_gg +
      ggplot2::annotate(
        geom = "text",
        x = 5,
        y = 1.5,
        size = 8, 
        label = title, 
        family = font_family
      )
  }
  airplane_gg
}