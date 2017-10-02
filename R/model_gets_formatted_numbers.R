#' @title Model becomes HTML
#'
#' @description Takes as input the output from \code{model_gets_formatted_numbers}.
#' @param tidy_model A dataframe.
#' @keywords internal






  model_gets_formatted_numbers <- function(univariate_models) {
    univariate_models %>%
      dplyr::transmute( variables,
                 categories,
                 estimate = format( round( estimate, decimals_estimate ), nsmall = decimals_estimate),
                 CI = paste0( format( round( conf.low,  decimals_estimate ), nsmall = decimals_estimate),
                              "-",
                              format( round( conf.high, decimals_estimate ), nsmall = decimals_estimate ) )
                 )  -> tidy_model

    tidy_model$estimate[ stringr::str_detect(  string = tidy_model$estimate,  pattern =  "NA") ] <- "1"
    tidy_model$CI[ stringr::str_detect(  string = tidy_model$CI,  pattern =  "NA") ]             <- "Ref"

    tidy_model
  }
