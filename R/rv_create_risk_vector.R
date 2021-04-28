#' Create risk vector based on ratio, number of places, or data frame
#'
#' @param risk_ratio A numeric vector or a data frame with two columns, `Risk` and `Ratio`
#' @param risk_places A numeric vector or a data frame with two columns, `Risk` and `Places`
#' @param risk_name If a numeric vector given in previous parameters, defines the name of the risk.
#' @param total_places Numeric. Gives the total length of the output vector. 
#'
#' @return A character vector with length equal to total_places.
#' @export
#'
#' @examples
#' 
#' 
#' rv_create_risk_vector(risk_ratio = 0.1,
#'                       total_places = 100)
#' table(
#'   rv_create_risk_vector(risk_ratio = 0.1,
#'                         total_places = 100) 
#' )
#' 
#' ratio <- tibble::tribble(~Risk, ~Ratio,
#'                          "Hospitalization", 0.3,
#'                          "Death", 0.1)
#' 
#' rv_create_risk_vector(risk_ratio = ratio,
#'                       total_places = 100)
#' 
#' table(
#'   rv_create_risk_vector(risk_ratio = ratio,
#'                         total_places = 100)
#' )
#' 
#' 


rv_create_risk_vector <- function(risk_ratio,
                                  total_places,
                                  risk_places = NULL,
                                  risk_names = NULL
) {
  
  if (is.null(risk_places)) {
    if (is.numeric(risk_ratio)==TRUE) {
      
      if (is.null(risk_names)) {
        risk_names <- stringr::str_c(risk_names, " ", seq_along(risk_ratio))
        risk_ratio <- tibble::tibble(Risk = risk_names,
                                     Ratio = risk_ratio)
      } else if (length(risk_names)==length(risk_ratio)) {
        risk_ratio <- tibble::tibble(Risk = risk_names,
                                     Ratio = risk_ratio)
      } else {
        risk_names <- stringr::str_c(risk_names[1], " ", seq_along(risk_ratio))
        risk_ratio <- tibble::tibble(Risk = risk_names,
                                     Ratio = risk_ratio)
      }
      
    } else {
      risk_names <-  risk_ratio %>%
        dplyr::filter(is.na(Risk)==FALSE) %>% 
        dplyr::pull(Risk) %>%
        unique()
    }
    
    total_ratio <- risk_ratio %>%
      dplyr::pull(2) %>%
      sum()
    
    if (total_ratio>1) {
      usethis::ui_stop("The sum of all elements of risk cannot be more than 1.")
    } else if (total_ratio==1) {
      
    } else if (total_ratio<1) {
      risk_ratio <- risk_ratio %>%
        dplyr::add_row(Risk = NA,
                       Ratio = 1-total_ratio)
    }
    risk_places_v <- rep(as.character(NA), total_places)
    
    for (i in (unique(risk_ratio[[1]])[is.na(unique(risk_ratio[[1]]))==FALSE])) {
      
      selector <- sample(x = seq_along(risk_places_v[is.na(risk_places_v)==TRUE]),
                         replace = FALSE,
                         size = round(total_places*risk_ratio$Ratio[risk_ratio$Risk==i&is.na(risk_ratio$Risk)==FALSE]))
      
      risk_places_v[is.na(risk_places_v)][selector] <- i
    }
    
  } else {
    if (is.numeric(risk_places)==TRUE) {
      risk_places <- tibble::tibble(Risk = stringr::str_c("Risk ", seq_along(risk_ratio)),
                                    Places = risk_places)
    }
    total_risk_places <- risk_places %>%
      dplyr::pull(2) %>%
      sum()
    if (total_risk_places>total_places) {
      usethis::ui_stop(x = "More risk places than total places available.")
    } else if (total_risk_places==total_places) {
      
    } else if (total_risk_places<total_places) {
      risk_places <- risk_places %>%
        dplyr::add_row(Risk = NA, Places = total_risk_places-total_risk_places)
    }
    
    risk_places_v <- rep(as.character(NA), total_places)
    for (i in (unique(risk_places[[1]])[is.na(unique(risk_places[[1]]))==FALSE])) {
      
      selector <- sample(x = seq_along(risk_places_v[is.na(risk_places_v)==TRUE]),
                         replace = FALSE,
                         size = risk_places[[2]][risk_places[[1]]==i])
      
      risk_places_v[is.na(risk_places_v)][selector] <- i
    }
  }
  
  
  if (is.factor(risk_places_v)==FALSE) {
    risk_places_v <- factor(x = risk_places_v, levels = risk_names)
  }
  
}
