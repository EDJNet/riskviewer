#' Create an arena with a given ratio of places highlighted
#'
#' @param ratio Numeric, between 0 and 1, required. Share of seats to colour.
#' @param ggplot Logical, defaults to FALSE. If TRUE, returns a `ggplot` object. If FALSE, a `magick` object.
#' @param bg Character vector of lenght 1, defines the background colour of the plot. Defaults to "white".
#'
#' @return
#' @export
#'
#' @examples
#'
#' rv_create_arena(ratio = 0.9)
#'
#' rv_create_arena(ratio = 0.1)
#'
rv_create_arena <- function(ratio,
                            title = NULL,
                            subtitle = NULL,
                            caption = NULL,
                            bg = "white",
                            quality = "low",
                            ggplot = FALSE) {
  ## how many seats per row?
  number_of_seats_first_row <- 164
  number_of_rows <- 5
  number_of_ramps <- 5


  seats_gradinata_df <- tibble::tibble(row = 5:sum(5, number_of_rows)) %>%
    dplyr::mutate(place = purrr::map(
      .x = row,
      .f = ~ 1:((number_of_ramps * 2) + number_of_seats_first_row * . / 5)
    )) %>%
    tidyr::unnest(place) %>%
    dplyr::group_by(row) %>%
    dplyr::mutate(location = place * 100 / max(place)) %>%
    dplyr::mutate(ramp = is.element(
      place,
      purrr::map_int(
        .x = cumsum(rep(
          x = (max(place) - min(place)) / number_of_ramps,
          times = number_of_ramps - 1
        )),
        .f = function(x) which.min(abs(place - x))
      )
    )) %>%
    dplyr::mutate(ramp = Reduce(
      f = `|`,
      x = list(ramp, dplyr::lag(ramp, n = 1))
    )) %>%
    dplyr::filter(ramp == FALSE | is.na(ramp)) %>%
    dplyr::mutate(place = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-ramp)


  seats_gradinata_gg_data <- dplyr::bind_rows(
    seats_gradinata_df,
    seats_gradinata_df %>% dplyr::mutate(row = row + 0.1),
    seats_gradinata_df %>% dplyr::mutate(row = row + 0.2),
    seats_gradinata_df %>% dplyr::mutate(row = row + 0.3),
    seats_gradinata_df %>% dplyr::mutate(row = row + 0.4),
    seats_gradinata_df %>% dplyr::mutate(row = row + 0.5)
  )


  ### Platea location ####

  number_of_seats_first_row <- 50
  number_of_rows <- 5
  number_of_ramps <- 5
  number_of_seats_last_row <- 36

  seats_platea_df <- tidyr::expand_grid(
    row = 1:number_of_rows,
    place = 1:sum(number_of_seats_first_row, number_of_ramps * 3)
  ) %>%
    dplyr::group_by(row) %>%
    dplyr::mutate(location = place * 100 / max(place)) %>%
    dplyr::mutate(ramp = is.element(
      place,
      purrr::map_int(
        .x = cumsum(rep(
          x = (max(place) - min(place)) / number_of_ramps,
          times = number_of_ramps - 1
        )),
        .f = function(x) which.min(abs(place - x))
      )
    )) %>%
    dplyr::mutate(ramp = Reduce(
      f = `|`,
      x = list(ramp, dplyr::lag(ramp, n = 1), dplyr::lead(ramp, n = 1))
    )) %>%
    dplyr::filter(ramp == FALSE | is.na(ramp)) %>%
    dplyr::mutate(place = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-ramp)

  seats_platea_gg_data <- dplyr::bind_rows(
    seats_platea_df,
    seats_platea_df %>% dplyr::mutate(row = row + 0.1),
    seats_platea_df %>% dplyr::mutate(row = row + 0.2),
    seats_platea_df %>% dplyr::mutate(row = row + 0.3),
    seats_platea_df %>% dplyr::mutate(row = row + 0.4),
    seats_platea_df %>% dplyr::mutate(row = row + 0.5)
  ) %>%
    dplyr::arrange(row, place) %>%
    dplyr::mutate(row_number = as.integer(factor(row))) %>%
    dplyr::mutate(seats_to_remove = max(place) - number_of_seats_last_row - row_number) %>%
    dplyr::mutate(seats_to_remove = dplyr::if_else(seats_to_remove < 0, 0, seats_to_remove)) %>%
    dplyr::mutate(remove = place >= (number_of_seats_first_row - seats_to_remove) | place <= (seats_to_remove)) %>%
    dplyr::filter(!remove)


  total_places <- nrow(seats_gradinata_gg_data) + nrow(seats_platea_gg_data)
  total_risk_places <- total_places * ratio

  gradinata_risk_places <- total_risk_places * nrow(seats_gradinata_gg_data) / total_places
  platea_risk_places <- total_risk_places * nrow(seats_platea_gg_data) / total_places


  gradinata_gg <-
    ggplot2::ggplot() +
    ggplot2::geom_point(
      data = seats_gradinata_gg_data %>%
        dplyr::mutate(risk_places = is.element(
          dplyr::row_number(),
          sample(
            x = 1:nrow(seats_gradinata_gg_data),
            size = gradinata_risk_places,
            replace = FALSE
          )
        )),
      mapping = ggplot2::aes(
        x = row,
        y = location + 0.25,
        colour = risk_places
      ),
      size = 0.3
    ) +
    ggplot2::scale_x_continuous(limits = c(0, sum(5.5, number_of_rows))) +
    ggplot2::scale_y_continuous(limits = c(0, 202)) +
    ggplot2::scale_colour_manual(values = c("grey80", "coral4")) +
    ggplot2::coord_polar(theta = "y", start = pi / 2) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")

  platea_gg <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = seats_platea_gg_data %>%
        dplyr::mutate(risk_places = is.element(dplyr::row_number(), sample(
          x = 1:nrow(seats_platea_gg_data),
          size = platea_risk_places,
          replace = FALSE
        ))),
      mapping = ggplot2::aes(
        x = location,
        y = row,
        colour = risk_places
      ), size = 0.3
    ) +
    ggplot2::scale_colour_manual(values = c("grey80", "coral4")) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")


  # https://shiny.rstudio.com/articles/images.html
  combo_gg <- cowplot::ggdraw() +
    cowplot::draw_plot(gradinata_gg) +
    cowplot::draw_plot(platea_gg,
      x = 0.32,
      y = 0.33,
      width = 0.36,
      height = 0.17
    )

  if (is.null(title) == FALSE) {
    combo_gg <- combo_gg +
      cowplot::draw_label(label = title, vjust = -0.5)
  }


  if (ggplot == TRUE) {
    return(combo_gg)
  } else {
    riskviewer::rv_img(
      plot = combo_gg,
      quality = quality,
      bg = bg
    )
  }
}
