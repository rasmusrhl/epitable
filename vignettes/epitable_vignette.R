## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
library(dplyr)
library(epitable)

## ------------------------------------------------------------------------
head(example_data)

## ---- echo=TRUE, results='asis'------------------------------------------
freq_by(dataset = example_data, var_vector = c("color", "clarity"), by_group = "cut" )

## ------------------------------------------------------------------------
freq_by(dataset = example_data, var_vector = c("color", "clarity"), by_group = "cut", font_css = "font-family: Times" )

## ---- results='asis'-----------------------------------------------------
 df         <- survival::lung
 df$age_bin <- Hmisc::cut2( df$age, g = 5)
 df$ph_bin  <- Hmisc::cut2( df$ph.karno, g = 5)
 df$sex     <- factor( df$sex)
 model1      <- survival::coxph( survival::Surv( time = time, event = status==1) ~ age_bin + factor(sex) + ph_bin + wt.loss, data = df )
 model_to_html(model1, exponentiate = TRUE )

## ------------------------------------------------------------------------
 diamonds <- ggplot2::diamonds
 diamonds$color <- factor(diamonds$color, ordered = FALSE)
 diamonds$clarity <- factor(diamonds$clarity, ordered = FALSE)
 glm_logistic <- glm( cut=="Ideal" ~  color + clarity + x , data = diamonds, family = "binomial")
 model_to_html(glm_logistic, exponentiate = TRUE)

## ------------------------------------------------------------------------
 glm_linear <- glm( Sepal.Width ~  Petal.Width + Sepal.Length + Petal.Length +  Species, data = iris)
 model_to_html(glm_linear) 

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
              exponentiate = TRUE ) 


