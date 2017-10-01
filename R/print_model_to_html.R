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

univariate_models_list <-  list( glm_logistic, glm_logistic ) 
univariate_models <- univariate_models_list

model_to_html <- function( univariate_models_list, simple = TRUE, decimals_estimate = 2 ) {

  # if(  ! "list" %in% class(univariate_models_list) ) {
  #   stop ("univariate_models_list must be a list")
  # }
  
  # if(model_class == "coxph") {
  #   purrr::map( univariate_models_list, class ) %>%
  #    purrr::map( function(x) "coxph" %in% x ) %>%
  #    unlist() %>%
  #    all() -> are_all_models_coxph
  #  if(! are_all_models_coxph ) stop ("When model_class is 'coxph' all models in univariate_models_list must be class 'coxph' ")
  # }
  
  
  tidy_up_model_df <- function(univariate_models) {
    univariate_models %>%
      transmute( variables,
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
  decimals_estimate <- 2

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

broom::tidy( glm_logistic, conf.int = TRUE ) 
broom::tidy( glm_linear, conf.int = TRUE  ) 
broom::tidy( model1 )
confint( glm_logistic)
broom::tidy( model1, )

tidy_model

list(model1) %>%
   purrr::map( add_reference_levels ) %>%
   purrr::map( tidy_up_model_df     ) %>%
   purrr::map( to_html              )
diamonds <- ggplot2::diamonds
diamonds$color <- factor(diamonds$color, ordered = FALSE)
diamonds$clarity <- factor(diamonds$clarity, ordered = FALSE)
glm_logistic <- glm( cut=="Ideal" ~  color + clarity + x , data = diamonds, family = "binomial")

glm_linear <- glm( Sepal.Width ~  Petal.Width + Species, data = iris)


model_to_html(list(model1)) -> htmloutput
model_to_html(list(glm_logistic)) -> htmloutput
model_to_html(list(model1)) -> htmloutput
tempfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".html")
write_file( htmloutput[[1]], tempfile)
utils::browseURL(tempfile)


tidy_model <- add_reference_levels(model1)   %>% tidy_up_model_df()

readr::write_file( htmloutput[[1]], "htmloutput.html")
utils::browseURL("htmloutput.html")

         css_matrix     <-
        matrix(data = "padding-left: 0.5cm; padding-right: 0.5cm;",
               nrow = nrow(table1),
               ncol = ncol(table1))
      css_matrix[, 1] <-
        "padding-left: 0.4cm; padding-right: 0.3cm;"

      htmlTable::htmlTable(
        x =  table1,
        rnames = FALSE,
        cgroup   = c_group_vektor,
        n.cgroup = n_c_group_vektor,
        rgroup   = rgroup_vektor,
        n.rgroup = n_rgroup_vektor,
        align = alignment_vektor,
        css.cell = css_matrix
 
        
        
tidy_up_model_df(tidy_model)



temp_test
out11$HR

univariate_models_list %>%
  purrr::map(  add_reference_levels )  %>% bind_rows() -> univariate_models



tidy_up_model_df(tidy_model) -> temp_test
tidy_model

format(round(x, 2), nsmall = 2)



to_html(tidy_model) -> htmloutput




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


model_to_html( list(   ) ) -> tidy_model
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

diamonds <- ggplot2::diamonds
diamonds$color <- factor(diamonds$color, ordered = FALSE)
diamonds$clarity <- factor(diamonds$clarity, ordered = FALSE)
glm_logistic <- glm( cut=="Ideal" ~  color + clarity + x , data = diamonds, family = "binomial")



# model list

model_list <- 

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

# expect error if covariates are of class "ordered"# test ordered model
testthat::expect_error( model_to_html( glm_logistic <- glm( cut=="Ideal" ~  color + clarity + x , data = ggplot2::diamonds, family = "binomial")))

} )




model_to_html( univariate_models_list = model_list_with_extra )





