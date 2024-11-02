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
  column_position <- tibble::tibble(
    column = LETTERS[1:seats_per_row],
    position = c(
      2:sum(seats_per_row / 2, 1),
      sum(seats_per_row / 2, 3):sum(seats_per_row / 2, 3, 2)
    )
  )

  tidyr::expand_grid(
    row = -1:-rows,
    column = LETTERS[1:seats_per_row]
  ) %>%
    dplyr::left_join(column_position, by = "column") %>%
    dplyr::mutate(seat_name = stringr::str_c(
      stringr::str_pad(
        string = paste0(column, row),
        width = 3, side = "both"
      ),
      "\n"
    ))
}
