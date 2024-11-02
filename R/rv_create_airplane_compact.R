#' Create a compact airplane
#'
#' @param risk_vector A character vector of length equal to the number of seats (rows*seats_per_row)
#' @param fill A character vector of colours.
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
rv_create_airplane_compact <- function(risk_ratio,
                                       risk_vector = NULL,
                                       risk_names = NULL,
                                       rows = 33,
                                       seats_per_row = 6,
                                       fill = c("coral1", "darkcyan", "darkgoldenrod1", "darkseagreen1", "lightskyblue1"),
                                       na_colour = "grey80",
                                       title = NULL,
                                       font_family = "sans",
                                       legend_position = "none") {
  base_seats <- rv_create_airplane_seats(
    rows = rows,
    seats_per_row = seats_per_row
  )

  total_places <- rows * seats_per_row


  if (is.null(risk_names)) {
    if (is.data.frame(risk_ratio)) {
      risk_names <- unique(risk_ratio[["Risk"]])
    } else {
      risk_names <- "Risk"
    }
  } else {
    # nothing
  }

  if (is.null(risk_vector) == FALSE) {
    risk_places_v <- risk_vector
  } else {
    risk_places_v <- rv_create_risk_vector(
      risk_ratio = risk_ratio,
      risk_name = risk_name,
      total_places = total_places
    )
  }


  seats_combo <- base_seats %>%
    dplyr::bind_cols(risk_places = risk_places_v)

  airplane_gg <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = seats_combo,
      mapping = ggplot2::aes(
        x = position,
        y = row,
        colour = risk_places
      ), shape = 15, size = 2
    ) +
    ggplot2::scale_colour_manual(values = fill, na.value = na_colour, na.translate = TRUE, drop = FALSE, breaks = levels) +
    ggplot2::geom_segment(mapping = ggplot2::aes(x = 1, y = -rows - 2, xend = 1, yend = -0.9), size = 2, color = "black", lineend = "round") +
    ggplot2::geom_segment(mapping = ggplot2::aes(x = 9, y = -rows - 2, xend = 9, yend = -0.9), size = 2, color = "black", lineend = "round") +
    ggplot2::geom_curve(mapping = ggplot2::aes(x = 1, y = -1, xend = 9, yend = -1), size = 2, color = "black", curvature = -1, lineend = "round", ncp = 20) +
    ggplot2::scale_y_continuous(
      name = "",
      limits = c(-rows - 2, 3.5),
      breaks = -1:-rows,
      labels = stringr::str_pad(string = 1:rows, width = 2),
      expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(name = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(
        size = 12,
        family = font_family
      ),
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      legend.position = legend_position,
      legend.title = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(0, 0, 0, 0, "lines")
    )

  if (is.null(title) == FALSE) {
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
