#' Create risk airplane
#'
#' @param rows Number of rows, defaults to 33 (found in commercial airplanes, and with 6 seats per rows it adds up to 198 seats in total).
#' @param risk_ratio Fraction of places that are subject to risk. For example, if set to 0.1 (the default), 10% of places will be considered subject to risk.
#' @param risk_places Number of risk places, defaults to NULL. If given, takes precedence over ratio.
#' @param legend_position Defaults to "none". 
#' @param fill A vector of colours. 
#' @param coord_ratio Numeric, defaults to 0.8, untested with anything else. Y/X ratio. 
#'
#' @return
#' @export
#'
#' @examples
rv_create_airplane <- function(risk_ratio = 0.1,
                               risk_places = NULL,
                               risk_vector = NULL,
                               compact = FALSE,
                               risk_names = "Risk",
                               fill = c("coral1", "darkcyan", "darkgoldenrod1", "darkseagreen1", "lightskyblue1"),
                               na_colour = "grey80",
                               rows = 33, 
                               seats_per_row = 6,
                               title = NULL, 
                               font_family = "sans",
                               font_size = 20,
                               font_family_seats = "mono",
                               seat_size_ratio = 1,
                               legend_position = "none", 
                               coord_ratio = 0.8) {
  
  seats <- rv_create_airplane_seats(rows = rows,
                                    seats_per_row = seats_per_row)
  
  total_places <- rows*seats_per_row
  
  if (is.null(risk_vector)==FALSE) {
    risk_places_v <- risk_vector
  } else {
    risk_places_v <- rv_create_risk_vector(risk_ratio = risk_ratio,
                                           risk_places = risk_places,
                                           risk_names = risk_names,
                                           total_places = total_places)
  }
  
  
  ## check
  # tibble::tibble(base_risk = risk_places_v) %>%
  #   dplyr::group_by(base_risk) %>%
  #   dplyr::count() %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(risk_ratio = n/sum(n))
  
  
  seats_combo <- seats %>%
    dplyr::bind_cols(risk_places = risk_places_v)
  
  if (isFALSE(compact)) {
    
    airplane_gg <- ggplot2::ggplot() +
      ggplot2::geom_label(
        data = seats_combo,
        mapping = ggplot2::aes(x = position,
                               y = row,
                               fill = risk_places,
                               colour = risk_places
        ),
        label = "\n    ",
        size = 2.2*seat_size_ratio,
        label.size = 1*seat_size_ratio,
        label.padding = grid::unit(0.15*seat_size_ratio, "lines"),
        family=font_family_seats) +
      ggplot2::geom_label(
        data = seats_combo,
        mapping = ggplot2::aes(x = position,
                               y = row,
                               label = column,
                               fill = risk_places),
        size = 2.8*seat_size_ratio,
        label.size = 0.3*seat_size_ratio,
        nudge_y = 0.12*seat_size_ratio,
        label.padding = grid::unit(0.26*seat_size_ratio, "lines"),
        family=font_family_seats, 
        fill = "white") +
      ggplot2::scale_fill_manual(values = fill, na.value = na_colour, na.translate = FALSE, drop = FALSE) +
      ggplot2::scale_colour_manual(values = shades::lightness(fill, shades::delta(-30)), na.value = shades::lightness(na_colour, shades::delta(-30)), guide = "none", drop = FALSE) +
      ggplot2::geom_segment(mapping = ggplot2::aes(x = 1, y = -rows-2, xend = 1, yend = -0.9), size = 3, color = "black", lineend = "round") +
      ggplot2::geom_segment(mapping = ggplot2::aes(x = 9, y = -rows-2, xend = 9, yend = -0.9), size = 3, color = "black", lineend = "round") +
      ggplot2::geom_curve(mapping = ggplot2::aes(x = 1, y = -1, xend = 9, yend =-1), size = 3, color = "black", curvature = -1.1, lineend = "round", ncp = 20) +
      ggplot2::scale_y_continuous(name = "",
                                  limits = c(-rows-2, 3.5),
                                  breaks = -1:-rows,
                                  labels = stringr::str_pad(string = 1:rows, width = 2),
                                  expand = c(0, 0)) +
      ggplot2::scale_x_continuous(name = "") +
      ggplot2::theme_minimal() +
      ggplot2::theme(text = ggplot2::element_text(size = font_size,
                                                  family = font_family),
                     panel.grid = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(hjust = 1),
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
          size = 12, 
          label = title, 
          family = font_family
        )
    } 
  } else if (isTRUE(compact)) {
    
    airplane_gg <- rv_create_airplane_compact(risk_ratio = risk_ratio,
                                              risk_vector = risk_vector,
                                              risk_names = NULL,
                                              rows = rows,
                                              seats_per_row = seats_per_row,
                                              fill = fill,
                                              na_colour = na_colour,
                                              title = title, 
                                              font_family = font_family,
                                              legend_position = legend_position)
  }
  
  
  airplane_gg +
    ggplot2::coord_equal(ratio = coord_ratio,
                         ylim = c(-rows-2, 5)) 
  
}




#' Spread the risk among more planes
#'
#' @param risk_ratio 
#' @param number_of_planes 
#' @param titles 
#' @param risk_places 
#' @param risk_vector 
#' @param risk_names 
#' @param fill 
#' @param na_colour 
#' @param rows 
#' @param seats_per_row Defaults to 6. Untested with anything different from 6.
#' @param title 
#' @param font_family 
#' @param font_family_seats 
#' @param legend_position 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' 
rv_create_airplane_combo <- function(risk_ratio = 0.1,
                                     number_of_planes = 2,
                                     risk_places = NULL,
                                     risk_vector = NULL,
                                     compact = FALSE,
                                     risk_names = NULL,
                                     fill = c("coral1", "darkcyan", "darkgoldenrod1", "darkseagreen1", "lightskyblue1"),
                                     na_colour = "grey80",
                                     rows = 33, 
                                     seats_per_row = 6,
                                     title = NULL, 
                                     font_family = "sans",
                                     font_family_seats = "mono", 
                                     legend_position = "none",
                                     ncol = NULL,
                                     nrow = NULL,
                                     guides = NULL){
  
  total_places <- rows*seats_per_row*number_of_planes
  
  v <- rv_create_risk_vector(risk_ratio = risk_ratio,
                             risk_places = risk_places,
                             risk_names = risk_names,
                             total_places = total_places)
  
  rv_l <- split(v, 1:number_of_planes)
  
  plots_l <- purrr::map(.x = rv_l,
                        .f = function(x) {
                          rv_create_airplane(risk_vector = unlist(x),
                                             fill = fill,
                                             compact = compact,
                                             na_colour = na_colour,
                                             rows = rows, 
                                             font_family = font_family,
                                             font_family_seats = font_family_seats, 
                                             legend_position = legend_position)
                        }) 
  
  if (is.null(guides)==FALSE) {
    plots_l %>% 
      patchwork::wrap_plots(ncol = ncol,
                            nrow = nrow,
                            guides = guides) &
      ggplot2::theme(legend.position=legend_position)
  } else {
    plots_l %>% 
      patchwork::wrap_plots(ncol = ncol,
                            nrow = nrow,
                            guides = guides) 
  }
  
  
}



#' Create animated version with multiple scenarios
#'
#' @param risk_ratio 
#' @param transition_length 
#' @param state_length 
#' @param risk_places 
#' @param risk_vector 
#' @param risk_names 
#' @param fill 
#' @param na_colour 
#' @param rows 
#' @param title 
#' @param font_family 
#' @param font_family_seats 
#' @param legend_position 
#' @param ncol 
#' @param nrow 
#' @param guides 
#'
#' @return
#' @export
#'
#' @examples
rv_create_airplane_animation <- function(risk_ratio = c(`Scenario A` = 0.01, `Scenario B` = 0.1, `Scenario C` = 0.3, `Scenario D` = 0.7),
                                         transition_length = 0.5,
                                         state_length = 1,
                                         risk_places = NULL,
                                         risk_vector = NULL,
                                         risk_names = NULL,
                                         fill = c("coral1", "darkgoldenrod1", "darkseagreen1", "lightskyblue1"),
                                         na_colour = "grey80",
                                         rows = 33, 
                                         seats_per_row = 6,
                                         title = NULL, 
                                         font_family = "sans",
                                         font_family_seats = "mono", 
                                         legend_position = "none",
                                         ncol = NULL,
                                         nrow = NULL,
                                         guides = NULL){
  
  total_places <- rows*seats_per_row
  seats <- rv_create_airplane_seats(rows = rows)
  
  if (is.data.frame(risk_ratio)) {
    seats_combo <- purrr::map2_dfr(.x = risk_ratio[["Ratio"]], .y = risk_ratio[["Risk"]],
                                   .f = function(x, y) {
                                     dplyr::bind_cols(seats, 
                                                      tibble::tibble(risk_places = rv_create_risk_vector(risk_ratio = x,
                                                                                                         total_places = total_places),
                                                                     scenario = y))
                                     
                                   })
  } else {
    seats_combo <- purrr::map2_dfr(.x = risk_ratio, .y = names(risk_ratio),
                                   .f = function(x, y) {
                                     dplyr::bind_cols(seats, 
                                                      tibble::tibble(risk_places = rv_create_risk_vector(risk_ratio = x,
                                                                                                         total_places = total_places),
                                                                     scenario = y))
                                     
                                   })
  }
  
  
  gg_airplane_animated <- rv_create_airplane_graph_compact(seats = seats_combo,
                                                           fill = fill,
                                                           na_colour = na_colour,
                                                           title = title, 
                                                           font_family = font_family,
                                                           legend_position = legend_position) +
    gganimate::transition_states(
      scenario,
      transition_length = transition_length,
      state_length = state_length
    ) +
    gganimate::enter_fade() + 
    gganimate::exit_shrink() +
    gganimate::ease_aes('sine-in-out') +
    ggplot2::labs(title = '{closest_state}') +
    ggplot2::theme(plot.title = ggplot2::element_text(family = font_family,
                                                      hjust = 0.5))
  
  gg_airplane_animated
}

