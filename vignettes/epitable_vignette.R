## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
library(epitable)
library(dplyr)

## ------------------------------------------------------------------------
head(example_data)

## ---- echo=TRUE, results='asis'------------------------------------------
freq_by(dataset = example_data, var_vector = c("color", "clarity"), by_group = "cut" )

## ---- results='asis'-----------------------------------------------------
 df         <- survival::lung
 df$age_bin <- Hmisc::cut2( df$age, g = 5)
 df$ph_bin  <- Hmisc::cut2( df$ph.karno, g = 5)
 df$sex     <- factor( df$sex)
 model1      <- survival::coxph( survival::Surv( time = time, event = status==1) ~ age_bin + factor(sex) + ph_bin + wt.loss, data = df )
 
 model_to_html(model1, exponentiate = TRUE )

## ------------------------------------------------------------------------
# 
#  diamonds <- ggplot2::diamonds
#  diamonds$color <- factor(diamonds$color, ordered = FALSE)
#  diamonds$clarity <- factor(diamonds$clarity, ordered = FALSE)
#  glm_logistic <- glm( cut=="Ideal" ~  color + clarity + x , data = diamonds, family = "binomial")
#  glm_linear <- glm( Sepal.Width ~  Petal.Width + Species, data = iris)


