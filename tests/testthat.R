# library(testthat)
# library(epitable)
#
# test_check("epitable")
#' @import testthat

# models to check
#
#  df         <- survival::lung
#  df$age_bin <- Hmisc::cut2( df$age, g = 5)
#  df$ph_bin  <- Hmisc::cut2( df$ph.karno, g = 5)
#  df$sex     <- factor( df$sex)
#  model1      <- survival::coxph( survival::Surv( time = time, event = status==1) ~ age_bin + factor(sex) + ph_bin + wt.loss, data = df )
#
# # check list
# # tets that the function works, that it works in a clean session so that %>% is not loaded.
#
#  # model_to_html(model1)
#
#  diamonds <- ggplot2::diamonds
#  diamonds$color <- factor(diamonds$color, ordered = FALSE)
#  diamonds$clarity <- factor(diamonds$clarity, ordered = FALSE)
#  glm_logistic <- glm( cut=="Ideal" ~  color + clarity + x , data = diamonds, family = "binomial")
#  glm_linear <- glm( Sepal.Width ~  Petal.Width + Species, data = iris)
#
#
# epitable:::model_gets_ref_levels(glm_linear)  %>%
# epitable:::model_gets_formatted_numbers()
#
# model_to_html(univariate_models_list = glm_logistic, exponentiate = TRUE )
# model_to_html(univariate_models_list = model1, exponentiate = TRUE )
#
# model_to_html(glm_logistic, exponentiate = TRUE) -> htmloutput
# model_to_html(glm_linear, exponentiate = TRUE) -> htmloutput
#
# model_to_html(univariate_models_list = glm_linear)
# model_to_html(univariate_models_list = glm_logistic)
#
# model_to_html(univariate_models_list = glm_logistic) -> htmloutput
#
#
# model_to_html((model1), exponentiate = FALSE) -> htmloutput
#
# df1 <- data.frame( test = paste0( c(-0.12, 0.20, -1.2), ", ", c(-0.12, 0.20, -1.2) ) )
#
# paste0( "[",
# stringr::str_pad( string = as.character(format( round( c(-0.12, 0.2, -1.2),2 ),nsmall=2)), width = 5,
#                   side = "left" ),
#         ",",
# stringr::str_pad( string = as.character(format( round( c(-0.12, 0.2, -1.2),2 ),nsmall=2)), width = 5,
#                   side = "left" ),
#         "]"  )
#
#
#
#
#
# # model_to_html(univariate_models_list = glm_linear) -> htmloutput
# model_to_html(univariate_models_list = glm_logistic, exponentiate = TRUE) -> htmloutput
# serif <- "font-family: Serif;"  # same as times
# arial <- "font-family: Arial;"
# model_to_html(univariate_models_list = model1,
#               exponentiate = FALSE, font_css = "font-family: monospace;" ) -> htmloutput
# tempfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".html")
# readr::write_file( htmloutput[[1]], tempfile)
# utils::browseURL(tempfile)
#


