# library(testthat)
# library(epitable)
#
# test_check("epitable")
#' @import testthat


#
# # check list
# # tets that the function works, that it works in a clean session so that %>% is not loaded.
# # check the behavior if the grouping variable has missing.
# # and likewise if any of the count variables have missing.
#  # model_to_html(model1)
#
#  diamonds <- ggplot2::diamonds
#  diamonds$color <- factor(diamonds$color, ordered = FALSE)
#  diamonds$clarity <- factor(diamonds$clarity, ordered = FALSE)
#  diamonds$`This is a very strange Variable` <- diamonds$color
#
# glm_logistic <- glm( cut=="Ideal" ~ `This is a very strange Variable`  + clarity + x , data = diamonds, family = "binomial")
# output <- model_to_html(univariate_models_list = glm_logistic, exponentiate = TRUE, font_css = "font-family: Times;" )
# tempfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".html")
# readr::write_file( output, tempfile)
# utils::browseURL(tempfile)
#
#
#
#
#
#
#
#
#  glm_linear <- glm( Sepal.Width ~  Petal.Width + Sepal.Length + Petal.Length +  Species, data = iris)
#
#
# epitable:::model_gets_ref_levels(glm_linear)  %>%
# epitable:::model_gets_formatted_numbers()
#
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
# ### Test 1
#  df         <- survival::lung
#  df$age_bin <- Hmisc::cut2( df$age, g = 5)
#  df$ph_bin  <- Hmisc::cut2( df$ph.karno, g = 5)
#  df$sex     <- factor( df$sex)
#  model1      <- survival::coxph( survival::Surv( time = time, event = status==1) ~ age_bin + factor(sex) + ph_bin + wt.loss, data = df )
#
#
# model_to_html(univariate_models_list = model1,
#               exponentiate = TRUE ) -> htmloutput
#
# df         <- survival::lung
# df$age_bin <- Hmisc::cut2( df$age, g = 5)
# df$ph_bin  <- Hmisc::cut2( df$ph.karno, g = 5)
# df$sex     <- factor( df$sex)
# model1      <- survival::coxph( survival::Surv( time = time, event = status==1) ~ age_bin + factor(sex) + ph_bin + wt.loss, data = df )
#
# model_to_html(model1, exponentiate = TRUE ) -> htmloutput
# tempfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".html")
# readr::write_file( htmloutput[[1]], tempfile)
# utils::browseURL(tempfile)
#
#
#
# ### Test 2 Logistic regression lists
# # univariate models in a list:
# c("Age" , "Embarked" , "Sex" , "Fare" , "Pclass") %>%
#   paste0( "Survived ~ ", . ) %>%
#   purrr::map( ~ glm( as.formula(.), data = titanic, family = "binomial" )) -> univar_list
#
# # Check logistic regression: multivariate models in a list:
# glm_logistic_1 <- glm( Survived ~  Age + Embarked, data = titanic, family = "binomial")
# glm_logistic_2 <- glm( Survived ~  Age + Embarked + Sex + Fare, data = titanic, family = "binomial")
# glm_logistic_3 <- glm( Survived ~  Age + Embarked + Sex + Fare + Pclass, data = titanic, family = "binomial")
# multi_model_list <- list(glm_logistic_1, glm_logistic_2,  glm_logistic_3 )
#
# model_to_html(univariate_models_list = univar_list,
#               multivariate_models_list = multi_model_list,
#               exponentiate = TRUE ) -> htmloutput
#
# tempfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".html")
# readr::write_file( htmloutput[[1]], tempfile)
# utils::browseURL(tempfile)
#
#
# ### Test 2 Logistic regression lists
#
#
# lung <- survival::lung
# c( "inst",  "age", "sex", "ph.ecog", "ph.karno", "pat.karno", "meal.cal", "wt.loss" )  %>%
#   paste0( " Surv( time, status==2) ~ ", . ) %>%
#   purrr::map( ~ coxph( as.formula(.), data = lung )) -> univariate_coxph_list
#
# coxph_model1 <- survival::coxph( survival::Surv( time, status==2) ~ inst + age + sex, lung)
# coxph_model2 <- survival::coxph( survival::Surv( time, status==2) ~ inst + age + sex + ph.ecog + ph.karno, lung)
# coxph_model3 <- survival::coxph( survival::Surv( time, status==2) ~ inst + age + sex + ph.ecog + ph.karno + pat.karno, lung)
# coxph_model4 <- survival::coxph( survival::Surv( time, status==2) ~ inst + age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, lung)
#
# multivariate_coxph_list <- list( coxph_model1, coxph_model2, coxph_model3, coxph_model4 )
#
#
# model_to_html( univariate_coxph_list, multivariate_models_list = multivariate_coxph_list )
# tempfile <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".html")
# readr::write_file( htmloutput[[1]], tempfile)
# utils::browseURL(tempfile)
#
#
# broom::tidy( coxph_model4)
#
# library(broom)
# library(dplyr)
# lung <- survival::lung
# lung$origin <- sample( c("Danish", "Non-Western Immigrant", "Western Immigrant"), size = nrow(lung), replace = TRUE)
# lung$origin <- factor( lung$origin)
# lung$origin <- relevel( lung$origin, ref = "Danish" )
#
# lung_non_western <- lung %>% filter( origin %in% c( "Danish", "Non-Western Immigrant")) %>% droplevels()
# lung_western <- lung %>% filter( origin %in% c("Danish", "Western Immigrant")) %>% droplevels()
#
# non_western_test1 <- survival::coxph( survival::Surv( time, status==2) ~ origin , lung_non_western)
# non_western_test2 <- survival::coxph( survival::Surv( time, status==2) ~ origin + inst + age , lung_non_western)
# non_western_test3 <- survival::coxph( survival::Surv( time, status==2) ~ origin + inst + age + sex + ph.ecog , lung_non_western)
# non_western_test4 <- survival::coxph( survival::Surv( time, status==2) ~ origin + inst + age + sex + ph.ecog + ph.karno , lung_non_western)
# non_western_test5 <- survival::coxph( survival::Surv( time, status==2) ~ origin + inst + age + sex + ph.ecog + ph.karno + pat.karno , lung_non_western)
# non_western_test6 <- survival::coxph( survival::Surv( time, status==2) ~ origin + inst + age + sex + ph.ecog + ph.karno + pat.karno + meal.cal, lung_non_western)
# non_western_test7 <- survival::coxph( survival::Surv( time, status==2) ~ origin + inst + age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, lung_non_western)
#
#
# western_test1 <- survival::coxph( survival::Surv( time, status==2) ~ origin , lung_western)
# western_test2 <- survival::coxph( survival::Surv( time, status==2) ~ origin + inst + age , lung_western)
# western_test3 <- survival::coxph( survival::Surv( time, status==2) ~ origin + inst + age + sex + ph.ecog , lung_western)
# western_test4 <- survival::coxph( survival::Surv( time, status==2) ~ origin + inst + age + sex + ph.ecog + ph.karno , lung_western)
# western_test5 <- survival::coxph( survival::Surv( time, status==2) ~ origin + inst + age + sex + ph.ecog + ph.karno + pat.karno , lung_western)
# western_test6 <- survival::coxph( survival::Surv( time, status==2) ~ origin + inst + age + sex + ph.ecog + ph.karno + pat.karno + meal.cal, lung_western)
# western_test7 <- survival::coxph( survival::Surv( time, status==2) ~ origin + inst + age + sex + ph.ecog + ph.karno + pat.karno + meal.cal + wt.loss, lung_western)
#
# list(
#  non_western_test1,
#  non_western_test2,
#  non_western_test3,
#  non_western_test4,
#  non_western_test5,
#  non_western_test6,
#  non_western_test7  )  -> non_western_list
#
# list(
#  western_test1,
#  western_test2,
#  western_test3,
#  western_test4,
#  western_test5,
#  western_test6,
#  western_test7
#  )  -> western_list
#
#
#
# non_western_list %>% purrr::map( .f = function(x) {
#
#   coef_in_model <- paste0( names(coef(x)), collapse = ", " )
#   tidy_model    <- broom::tidy(x, exponentiate = TRUE) %>% head(1)
#   tidy_model$names1 <- coef_in_model
#   tidy_model %>% select( Covariates = names1, HR = estimate, conf.low, conf.high)
# }                               ) %>% bind_rows()  %>%
#
#   transmute( Covariates, HR = round(HR, 2), CI = paste0( format( round( conf.low, 2),nsmall = 2), " - ", format( round( conf.high, 2),nsmall = 2)) )  -> left_table
#
#
# western_list %>% purrr::map( .f = function(x) {
#
#   coef_in_model <- paste0( names(coef(x)), collapse = ", " )
#   tidy_model    <- broom::tidy(x, exponentiate = TRUE) %>% head(1)
#   tidy_model$names1 <- coef_in_model
#   tidy_model %>% select( Covariates = names1, HR = estimate, conf.low, conf.high)
# }                               ) %>% bind_rows()  %>%
#
#   transmute( Covariates, HR = round(HR, 2), CI = paste0( format( round( conf.low, 2),nsmall = 2), " - ", format( round( conf.high, 2),nsmall = 2)) )  -> right_table
#
#
# bind_cols( left_table,  right_table[,-1] )
#
#
#
#
