## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
library(dplyr)
library(epitable)
library(knitr)
knitr::opts_chunk$set( cache = TRUE )

## ------------------------------------------------------------------------
head(example_data) 

## ------------------------------------------------------------------------
freq_by(dataset = example_data, var_vector = c("color", "clarity"), by_group = "cut", font_css = "font-family: Times" )

## ------------------------------------------------------------------------
# univariate models in a list:
c("Age" , "Embarked" , "Sex" , "Fare" , "Pclass") %>%
  paste0( "Survived ~ ", . ) %>%
  purrr::map( ~ glm( as.formula(.), data = titanic, family = "binomial" )) -> univar_list

# multivariate models in a list:
glm_logistic_1 <- glm( Survived ~  Age + Embarked, data = titanic, family = "binomial")
glm_logistic_2 <- glm( Survived ~  Age + Embarked + Sex + Fare, data = titanic, family = "binomial")
glm_logistic_3 <- glm( Survived ~  Age + Embarked + Sex + Fare + Pclass, data = titanic, family = "binomial")
multi_model_list <- list(glm_logistic_1, glm_logistic_2,  glm_logistic_3 )

model_to_html(univariate_models_list = univar_list,
              multivariate_models_list = multi_model_list,
              exponentiate = TRUE, html_output = TRUE ) 


## ---- results='asis'-----------------------------------------------------
 df         <- dplyr::as_tibble(survival::lung)
 df$age_bin <- Hmisc::cut2( df$age, g = 5)
 df$ph_bin  <- Hmisc::cut2( df$ph.karno, g = 5)
 df$sex     <- factor( df$sex)
 model1      <- survival::coxph( survival::Surv( time = time, event = status==1) ~ age_bin + sex + ph_bin + wt.loss, data = df )
 model_to_html(model1, exponentiate = TRUE )
 

## ----eval=FALSE----------------------------------------------------------
#  <style type="text/css">
#  .main-container {
#    max-width: 1400px;
#    margin-left: auto;
#    margin-right: auto;
#  }
#  </style>

