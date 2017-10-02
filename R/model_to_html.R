#' @title Print regression models to HTML
#'
#' @description Prints on or more lists of regression models to HTML. Currently
#'     only works with class coxph.
#' @param univariate_models_list List of univariate models
#' @param decimals_estimate A number specifying decimals on estimates. Default is 2.
#' @importFrom Hmisc cut2
#' @importFrom dplyr "%>%"
#' @import survival
#' @export model_to_html
#' @examples
#' df         <- survival::lung
#' df$age_bin <- Hmisc::cut2( df$age, g = 5)
#' df$ph_bin  <- Hmisc::cut2( df$ph.karno, g = 5)
#' df$sex     <- factor( df$sex)
#' model1     <- survival::coxph( survival::Surv( time , status==1) ~ ph_bin + wt.loss, data = df)
#'
#' diamonds         <- example_data
#' diamonds$color   <- factor(diamonds$color, ordered = FALSE)
#' diamonds$clarity <- factor(diamonds$clarity, ordered = FALSE)
#' glm_logistic     <- glm( cut=="Ideal" ~  color + clarity + x , data = diamonds, family = "binomial")
#' glm_linear       <- glm( Sepal.Width ~  Petal.Width + Species, data = iris)


model_to_html <- function( univariate_models_list, decimals_estimate = 2, exponetiate = FALSE ) {

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


 univariate_models_list %>%
   purrr::map( epitable:::model_gets_ref_levels ) %>%
   purrr::map( epitable:::model_gets_formatted_numbers     ) %>%
   purrr::map( epitable:::model_becomes_html              )

}



