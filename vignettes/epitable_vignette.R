## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
library(dplyr)
library(epitable)
library(knitr)
knitr::opts_chunk$set( cache = FALSE )

## ------------------------------------------------------------------------
head(example_data) 

## ------------------------------------------------------------------------
freq_by(dataset = example_data, var_vector = c("color", "clarity"), by_group = "cut" )

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


## ------------------------------------------------------------------------
model_to_html( univar_list,  multi_model_list, exponentiate = TRUE )

## ------------------------------------------------------------------------
lung <- survival::lung
c( "inst",  "age", "sex", "ph.ecog", "ph.karno", "pat.karno", "meal.cal", "wt.loss" )  %>%
  paste0( " survival::Surv( time, status==2) ~ ", . ) %>%
  purrr::map( ~ survival::coxph( as.formula(.), data = lung )) -> univariate_coxph_list

coxph_model1 <- survival::coxph( survival::Surv( time, status==2) ~ inst + age + sex, lung)
coxph_model2 <- survival::coxph( survival::Surv( time, status==2) ~ inst + age + sex + ph.ecog + ph.karno, lung)
coxph_model3 <- survival::coxph( survival::Surv( time, status==2) ~ inst + age + sex + ph.ecog + ph.karno + pat.karno, lung)
coxph_model4 <- survival::coxph( survival::Surv( time, status==2) ~ inst + age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, lung)

multivariate_coxph_list <- list( coxph_model1, coxph_model2, coxph_model3, coxph_model4 )
model_to_html( univariate_coxph_list, multivariate_models_list = multivariate_coxph_list )

## ------------------------------------------------------------------------
model_to_html( univariate_coxph_list, multivariate_models_list = multivariate_coxph_list, font_css = "font-family: Times;", decimals_estimate = 4)

## ------------------------------------------------------------------------
freq_by(dataset = example_data, var_vector = c("color", "clarity"), by_group = "cut", font_css = "font-family: Arial" )

## ----eval=FALSE----------------------------------------------------------
#  <style type="text/css">
#  .main-container {
#    max-width: 1400px;
#    margin-left: auto;
#    margin-right: auto;
#  }
#  </style>

