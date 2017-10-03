# library(testthat)
# library(epitable)
#
# test_check("epitable")
#' @import testthat

# models to check

 df         <- survival::lung
 df$age_bin <- Hmisc::cut2( df$age, g = 5)
 df$ph_bin  <- Hmisc::cut2( df$ph.karno, g = 5)
 df$sex     <- factor( df$sex)
 model1      <- survival::coxph( survival::Surv( time = time, event = status==1) ~ age_bin + factor(sex) + ph_bin + wt.loss, data = df )

# check list
# tets that the function works, that it works in a clean session so that %>% is not loaded.

 # model_to_html(model1)

 diamonds <- ggplot2::diamonds
 diamonds$color <- factor(diamonds$color, ordered = FALSE)
 diamonds$clarity <- factor(diamonds$clarity, ordered = FALSE)
 glm_logistic <- glm( cut=="Ideal" ~  color + clarity + x , data = diamonds, family = "binomial")
 glm_linear <- glm( Sepal.Width ~  Petal.Width + Sepal.Length + Petal.Length +  Species, data = iris)


epitable:::model_gets_ref_levels(glm_linear)  %>%
epitable:::model_gets_formatted_numbers()

model_to_html(univariate_models_list = glm_logistic, exponentiate = TRUE )
model_to_html(univariate_models_list = model1, exponentiate = TRUE )

model_to_html(glm_logistic, exponentiate = TRUE) -> htmloutput
model_to_html(glm_linear, exponentiate = TRUE) -> htmloutput

model_to_html(univariate_models_list = glm_linear)
model_to_html(univariate_models_list = glm_logistic)

model_to_html(univariate_models_list = glm_logistic) -> htmloutput


model_to_html((model1), exponentiate = FALSE) -> htmloutput

df1 <- data.frame( test = paste0( c(-0.12, 0.20, -1.2), ", ", c(-0.12, 0.20, -1.2) ) )

paste0( "[",
stringr::str_pad( string = as.character(format( round( c(-0.12, 0.2, -1.2),2 ),nsmall=2)), width = 5,
                  side = "left" ),
        ",",
stringr::str_pad( string = as.character(format( round( c(-0.12, 0.2, -1.2),2 ),nsmall=2)), width = 5,
                  side = "left" ),
        "]"  )




# univariate models in a list:
c("Age" , "Embarked" , "Sex" , "Fare" , "Pclass") %>%
  paste0( "Survived ~ ", . ) %>%
  purrr::map( ~ glm( as.formula(.), data = titanic, family = "binomial" )) -> univar_list

# multivariate models in a list:
glm_logistic_1 <- glm( Survived ~  Age + Embarked, data = titanic, family = "binomial")
glm_logistic_2 <- glm( Survived ~  Age + Embarked + Sex + Fare, data = titanic, family = "binomial")
glm_logistic_3 <- glm( Survived ~  Age + Embarked + Sex + Fare + Pclass, data = titanic, family = "binomial")

multi_model_list <- list(glm_logistic_1, glm_logistic_2,  glm_logistic_3 )

model_to_html(univariate_models_list = glm_linear, exponentiate = TRUE, cgroup_names = "lalaa") -> htmloutput
tempfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".html")
readr::write_file( htmloutput[[1]], tempfile)
utils::browseURL(tempfile)

model_becomes_html(htmloutput)



model_becomes_html           <- function( tidy_model        ) {

  # tidy_model <- htmloutput # delete me
  # cgroup_names <- " " # delete me
  # n_cgroup     <- 3 # delete me
  # font_css     <- "font-family: monospace;" # delete me

  rgroup_vector       <-   stringr::str_to_title( rle(tidy_model$variables)$values )
  n_rgroup_vector     <-   rle(tidy_model$variables)$lengths
  rgroup_vector[ n_rgroup_vector == 1 ]   <-  " " #"&nbsp;" # single rows dont need rgroup header

  css_rgroup      <- "font-style: italic;padding-top: 0.4cm;padding-right: 0.1cm;padding-bottom: 0.01cm;"
  tidy_model      <- tidy_model[,-1]
  css_matrix      <- matrix(data = "padding-left: 0.5cm; padding-right: 0.5cm;",
                            nrow = nrow(tidy_model),
                            ncol = ncol(tidy_model))
  css_matrix[, 1] <- "padding-left: 0.6cm; padding-right: 0.3cm;"

  names(tidy_model)[  names(tidy_model)=="categories"] <- "&nbsp;"


  htmlTable::htmlTable(
    x          = tidy_model ,
    rnames     = FALSE,
    rgroup     = rgroup_vector,
    n.rgroup   = n_rgroup_vector,
    align      = c("l","r"),
    css.rgroup = css_rgroup,
    css.cell   = css_matrix,
    css.table  = font_css,
    cgroup     = cgroup_names,
    n.cgroup   = n_cgroup
  ) -> output_htmltable
  output_htmltable
}







