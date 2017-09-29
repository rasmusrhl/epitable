#' @title Print regression models to HTML
#'
#' @description Prints on or more lists of regression models to HTML. Currently
#'     only works with class coxph.
#' @param univariate_models_list List of univariate models
#' @importFrom Hmisc cut2
#' @export print_model_to_html
#'
#'

library(dplyr)




model_to_html <- function( univariate_models_list, model_class = "coxph" ) {

  if(! is.list(univariate_models_list)) {
    stop ("univariate_models_list must be a list")
  }

  if(model_class == "coxph") {
    purrr::map( univariate_models_list, class ) %>%
     purrr::map( function(x) "coxph" %in% x ) %>%
     unlist() %>%
     all() -> are_all_models_coxph
   if(! are_all_models_coxph ) stop ("When model_class is 'coxph' all models in univariate_models_list must be class 'coxph' ")
  }

univariate_models_list %>%
  purrr::map(  add_reference_levels )

}

model_to_html(model_list)

add_reference_levels(model1 )

df %>% count(ph_bin)

# Test function -----------------------------------------------------------


# test data
library(survival)
df$age_bin <- Hmisc::cut2( df$age, g = 5)
df$ph_bin  <- Hmisc::cut2( df$ph.karno, g = 5)
df$sex     <- factor( df$sex)

# test models

# single model
model1      <- survival::coxph( survival::Surv( time = time, event = status==1) ~ age_bin + factor(sex) + ph_bin + wt.loss, data = df )

# model list
model_list  <-  c("age_bin", "sex", "ph_bin" ) %>%
                    purrr::map( function(x) paste( "survival::Surv( time = time, event = status==1) ~ ", x ) ) %>%
                    purrr::map( function(x) survival::coxph( as.formula(x), data = df))
# model list with element of class not "coxph"
model_list_with_extra      <- model_list
model_list_with_extra[[4]] <- iris


# test unit: user input
test_that("User input is correct",  {

# expect error if univariate_models_list is not list
testthat::expect_error( model_to_html( univariate_models_list = "asdf") )

# expect error if univariate_models_list contain not coxph element
testthat::expect_error( model_to_html( univariate_models_list = model_list_with_extra ))
} )




model_to_html( univariate_models_list = model_list_with_extra )





