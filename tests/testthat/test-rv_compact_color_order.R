test_that("order of colors is kept", {
  risk_ratio <- tibble::tribble(
    ~Risk, ~Ratio,
    "Green", 0.01,
    "Blue", 0.04,
    "Red", 0.08,
    "Pink", 0.16
  )


  rv_create_airplane_compact(
    risk_ratio = risk_ratio,
    fill = c("green", "blue", "red", "pink"),
    na_colour = "grey80"
  )
})



test_that("multiplication works", {
  risk_ratio <- tibble::tribble(
    ~Risk, ~Ratio,
    "Green", 0.01,
    "Blue", 0.04,
    "Red", 0.08,
    "Yellow", 0.16
  )

  #
  # risk_vector = NULL
  # risk_names = NULL
  # risk_places = NULL
  # rows = 33
  # seats_per_row = 6
  # fill = c("green", "blue", "red", "pink")
  # na_colour = "grey80"
  # title = NULL
  # font_family = "sans"
  # legend_position = "none"
  #
  rv_create_airplane_combo(
    risk_ratio = risk_ratio,
    number_of_planes = 10,
    compact = TRUE,
    fill = c("green", "blue", "red", "yellow"),
    nrow = 2,
    ncol = 5
  )
})
