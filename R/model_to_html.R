#' @title Print regression models to HTML
#'
#' @description Prints on or more lists of regression models to HTML. Currently
#'     only works with class coxph.
#' @param univariate_models_list List of univariate models
#' @param decimals_estimate A number specifying decimals on estimates. Default is 2.
#' @importFrom Hmisc cut2
#' @import ggplot2 survival
#' @export model_to_html
#' @examples
#' df         <- survival::lung
#' df$age_bin <- Hmisc::cut2( df$age, g = 5)
#' df$ph_bin  <- Hmisc::cut2( df$ph.karno, g = 5)
#' df$sex     <- factor( df$sex)
#' model1     <- survival::coxph( survival::Surv( time = time, event = status==1) ~ age_bin + factor(sex) + ph_bin + wt.loss, data = df )
#'
#' diamonds         <- ggplot2::diamonds
#' diamonds$color   <- factor(diamonds$color, ordered = FALSE)
#' diamonds$clarity <- factor(diamonds$clarity, ordered = FALSE)
#' glm_logistic     <- glm( cut=="Ideal" ~  color + clarity + x , data = diamonds, family = "binomial")
#' glm_linear       <- glm( Sepal.Width ~  Petal.Width + Species, data = iris)


model_to_html <- function( univariate_models_list, decimals_estimate = 2 ) {

  if(  ! "list" %in% class(univariate_models_list) ) { # input must be list dont know why
    univariate_models_list <- list(univariate_models_list)
  }

  # if(model_class == "coxph") {
  #   purrr::map( univariate_models_list, class ) %>%
  #    purrr::map( function(x) "coxph" %in% x ) %>%
  #    unlist() %>%
  #    all() -> are_all_models_coxph
  #  if(! are_all_models_coxph ) stop ("When model_class is 'coxph' all models in univariate_models_list must be class 'coxph' ")
  # }


  tidy_up_model_df <- function(univariate_models) {
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


  to_html <- function(tidy_model) {

    rgroup_vector       <-   stringr::str_to_title( rle(tidy_model$variables)$values )
    n_rgroup_vector     <-   rle(tidy_model$variables)$lengths
    rgroup_vector[ n_rgroup_vector == 1 ]   <- "&nbsp;" # single rows dont need rgroup header

    css_rgroup      <- "font-style: italic;padding-top: 0.4cm;padding-right: 0.4cm;padding-bottom: 0.2cm;"
    tidy_model      <- tidy_model[,-1]
    css_matrix      <- matrix(data = "padding-left: 0.5cm; padding-right: 0.5cm;",
                              nrow = nrow(tidy_model),
                              ncol = ncol(tidy_model))
    css_matrix[, 1] <- "padding-left: 0.4cm; padding-right: 0.3cm;"

    htmlTable::htmlTable(
     x          = tidy_model ,
     rnames     = FALSE,
     rgroup     = rgroup_vector,
     n.rgroup   = n_rgroup_vector,
     align      = c("l","r"),
     css.rgroup = css_rgroup,
     css.cell   = css_matrix
    )
  }


 univariate_models_list %>%
   purrr::map( add_reference_levels ) %>%
   purrr::map( tidy_up_model_df     ) %>%
   purrr::map( to_html              )

}



