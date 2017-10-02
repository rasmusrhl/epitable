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

 diamonds <- ggplot2::diamonds
 diamonds$color <- factor(diamonds$color, ordered = FALSE)
 diamonds$clarity <- factor(diamonds$clarity, ordered = FALSE)
 glm_logistic <- glm( cut=="Ideal" ~  color + clarity + x , data = diamonds, family = "binomial")
 glm_linear <- glm( Sepal.Width ~  Petal.Width + Species, data = iris)


model_to_html(univariate_models_list = glm_logistic)
model_to_html(univariate_models_list = glm_linear)
model_to_html(univariate_models_list = model1)

model_to_html(list(model1)) -> htmloutput
model_to_html(list(glm_logistic)) -> htmloutput
model_to_html(list(model1)) -> htmloutput
tempfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".html")
readr::write_file( htmloutput[[1]], tempfile)
utils::browseURL(tempfile)

