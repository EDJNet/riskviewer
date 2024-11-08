% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rv_airplane.R
\name{rv_create_airplane_animation}
\alias{rv_create_airplane_animation}
\title{Create animated version with multiple scenarios}
\usage{
rv_create_airplane_animation(
  risk_ratio = c(`Scenario A` = 0.01, `Scenario B` = 0.1, `Scenario C` = 0.3,
    `Scenario D` = 0.7),
  transition_length = 0.5,
  state_length = 1,
  risk_places = NULL,
  risk_vector = NULL,
  risk_names = NULL,
  fill = c("coral1", "darkgoldenrod1", "darkseagreen1", "lightskyblue1"),
  na_colour = "grey90",
  rows = 33,
  seats_per_row = 6,
  title = NULL,
  font_family = "sans",
  font_family_seats = "mono",
  legend_position = "none",
  ncol = NULL,
  nrow = NULL,
  guides = NULL
)
}
\arguments{
\item{guides}{}
}
\description{
Create animated version with multiple scenarios
}
