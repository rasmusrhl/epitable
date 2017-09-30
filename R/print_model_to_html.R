#' @title Print regression models to HTML
#'
#' @description Prints on or more lists of regression models to HTML. Currently
#'     only works with class coxph.
#' @param univariate_models_list List of univariate models
#' @param decimals_estimate A number specifying decimals on estimates. Default is 2.
#' @importFrom Hmisc cut2
#' @export print_model_to_html
#'
#'

library(dplyr)
library(purrr)

library(tidyverse)


model_to_html <- function( univariate_models_list, model_class = "coxph", simple = TRUE, decimals_estimate = 2 ) {

  if(  !class(univariate_models_list)=="list") {
    stop ("univariate_models_list must be a list")
  }

  if(model_class == "coxph") {
    purrr::map( univariate_models_list, class ) %>%
     purrr::map( function(x) "coxph" %in% x ) %>%
     unlist() %>%
     all() -> are_all_models_coxph
   if(! are_all_models_coxph ) stop ("When model_class is 'coxph' all models in univariate_models_list must be class 'coxph' ")
  }
  
  
  tidy_up_model_df <- function(univariate_models) {
    univariate_models %>%
      transmute( variables,
                 categories,
                 estimate = format( round( estimate, decimals_estimate ), nsmall = decimals_estimate),
                 CI = paste0( format( round( conf.low, decimals_estimate),nsmall= decimals_estimate),
                              "-", 
                              format( round( conf.high, decimals_estimate ), nsmall = decimals_estimate )) ,
                 )  -> tidy_model
    
    tidy_model$estimate[ stringr::str_detect(  string = tidy_model$estimate,  pattern =  "NA") ] <- "1" 
    tidy_model$CI[ stringr::str_detect(  string = tidy_model$CI,  pattern =  "NA") ]             <- "Ref" 
    
    tidy_model
  }

  
  to_html <- function(tidy_model) {
    
    rle_1   <-   rle(tidy_model$variables)
    rle_1$values[  which( rle_1$lengths == 1 )  ]  <- "&nbsp;" # single rows dont nead rgroup header
    
    
    htmlTable::htmlTable( 
     tidy_model , 
     rnames   = FALSE,
     rgroup   = rle_1$values,
     n.rgroup = rle_1$lengths,
     align    = c("l","r") 
    ) 
  }
   add_reference_levels(univariate_models_list) %>%
   tidy_up_model_df() %>%
    to_html()

}


model_to_html(list(model1))

tidy_up_model_df(tidy_model)



temp_test
out11$HR

univariate_models_list %>%
  purrr::map(  add_reference_levels )  %>% bind_rows() -> univariate_models



tidy_up_model_df(tidy_model) -> temp_test
tidy_model

format(round(x, 2), nsmall = 2)



to_html(tidy_model) -> htmloutput

readr::write_file( htmloutput, "htmloutput.html")
utils::browseURL("htmloutput.html")


htmlTable::htmlTable(
  x =  table1,
  rnames = FALSE,
  cgroup   = c_group_vektor,
  n.cgroup = n_c_group_vektor,
  rgroup   = rgroup_vektor,
  n.rgroup = n_rgroup_vektor,
  align = alignment_vektor,
  css.rgroup = "font-style: italic;padding-top: 0.4cm;padding-right: 0.4cm;padding-bottom: 0.2cm;",
  css.cell = css_matrix
)


model_to_html( list(model1  ) ) -> tidy_model
readr::write_file( tidy_model, "asdf.html")
utils::browseURL("asdf.html")

model_to_html(model_list) 




# Test function -----------------------------------------------------------


# test data
library(survival)
df         <- lung
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





