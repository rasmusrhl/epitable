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

univariate_models_list %>%
  purrr::map(  add_reference_levels )  %>% bind_rows() -> univariate_models

univariate_models

}

decimals_estimate <- 2

tidy_up_model_df <- function(univariate_models) {
  tidy_model %>%
    transmute( variables,
               categories,
               HR = round( estimate, decimals_estimate ),
               conf.low, "-" conf.high )
               )
 tidy_model %>% head()  
}

to_html <- function(tidy_model) {
  
rle_1   <-   rle(tidy_model$variables)
rle_1$values[  which( rle_1$lengths == 1 )  ]  <- "&nbsp;" # single rows dont nead rgroup header

to_html <- tidy_model %>% select( categories, estimate, conf.low, conf.high )
htmlTable::htmlTable( 
 tidy_model , 
 rnames   = FALSE,
 rgroup   = rle_1$values,
 n.rgroup = rle_1$lengths,
 align    = c("l","r")
 
) 
}

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

model_to_html(model_list) 



table1 <- table1[, -1] # exclude first column, because it is shown in the rgroup.
names(table1) <-
  c(" ", rep(c("n", "(%)"), number_of_summaries_in_table))

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
  css.rgroup = "font-style: italic;padding-top: 0.4cm;padding-right: 0.4cm;padding-bottom: 0.2cm;",
  css.cell = css_matrix
)



model1$n
df %>% count(sex)
df %>% count(ph_bin)
model1
str(model1)
attr(model1, 

model_to_html(model_list) %>% bind_rows()
class( list(model1  ) )  == "list"

class( model1 )
output %>% bind_rows()

out1

print( round( output$conf.high, 3) , na.print = "-"  )


add_reference_levels(model1 ) 
add_reference_levels(model1 ) -> output

df %>% count(ph_bin)

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





