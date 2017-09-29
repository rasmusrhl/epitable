#' @title Print regression models to HTML
#'
#' @description Prints on or more lists of regression models to HTML. Currently
#'     only works with class coxph.
#' @param univariate_models_list List of univariate models
#' @importFrom Hmisc cut2
#' @export print_model_to_html
#'
#'

library(survival)
library(dplyr)
df$age_bin <- Hmisc::cut2( df$age, g = 5)
df$ph_bin  <- Hmisc::cut2( df$ph.karno, g = 5)
df$sex     <- factor( df$sex)
model1     <- survival::coxph( survival::Surv( time = time, event = status==1) ~ age_bin + factor(sex) + ph_bin, data = df )




 c("age_bin", "sex", "ph_bin" ) %>%
  purrr::map( function(x) paste( "survival::Surv( time = time, event = status==1) ~ ", x ) ) %>%
  purrr::map( function(x) survival::coxph( as.formula(x), data = df)) -> model_list


print_model_to_html <- function( univariate_models_list ) {

  if(! univariate_models_list) {
    stop ("univariate mdels list")
  }

}

print_model_to_html( model_list)


is.list(model_list)
