#'
#'
#' @description A new version of \code{freq_by} which has more features.
#' @param dataset A dataset
#' @param var_vector A character vector referring to column names in the
#'     \code{dataset}.
#' @param by_group A string referring to a column name in the \code{dataset}.
#'     Must be of type \code{factor}.
#'
#'
#'
#'


library(tidyverse)
library(rlang)
library(broom)
# test data
diamonds  <- ggplot2::diamonds
diamonds$group_var   <- sample( x = c("Group 1", "Group 2", "Group 3"), size = nrow(diamonds), replace = TRUE)
diamonds$cut[100:200] <- NA
var_vector <- c("cut","color", "price" )

# function
freq_by <- function(dataset, var_vector, by_group = NULL, include_total = TRUE, min_cell_count = 10, htmlout = TRUE, font_css = "font-family: monospace;",
                    include_p_value = FALSE,
                    decimal_percent = 0,
                    include_n = TRUE,
                    include_percent = TRUE,
                    include_cumpct = FALSE,
                    include_cumsum = FALSE,
                    include_subtotal = FALSE,
                    include_n_missing = FALSE) {

# Initial checks and conversion -------------------------------------------


by_group_symbol         <- rlang::sym(by_group)
var_vector_numeric      <- dataset[, var_vector ] %>% dplyr::select_if( is.numeric ) %>% names()
var_vector_char         <- dataset[, var_vector ] %>% dplyr::select_if( function(x) is.factor(x) | is.character(x) ) %>% names()
min_cell_count <- 5



freq_fun_by <- function(dataset, var_vector_char_element) {
char_element_symbol  <-  rlang::sym(var_vector_char_element)
by_group_vector          <-  dataset               %>% pull(UQ(by_group_symbol))
var_vector_element_pull  <-  dataset               %>% pull(UQ(var_vector_char_element))
# test
  chi_test               <- chisq.test( x = by_group_vector, y = var_vector_element_pull  ) %>% tidy()
  chi_test$var_vector    <- var_vector_char_element

# count

  dataset %>% count(UQ(by_group_symbol), UQ(char_element_symbol)) %>% complete( UQ(by_group_symbol), UQ(char_element_symbol), fill = list( n = 0 )) %>%

   group_by(UQ(by_group_symbol)) %>%

    mutate( "group" = UQ(by_group_symbol)[1],
            "covariate"    = var_vector_char_element,
            "category"     = UQ(char_element_symbol),
            "pct"          = paste0( round( 100 * n / sum(n), decimal_percent), "%" ),
            "cumpct"       = paste0( round( 100 * cumsum(n) / sum(n), decimal_percent), "%" ),
            "cumsum"       = prettyNum(cumsum(n), big.mark = " " ),
            "subtotal"     = ifelse(  dplyr::row_number(covariate)==1, yes = prettyNum( sum(n), big.mark = " "), no = NA),
            "n_missing"    = ifelse(  dplyr::row_number(covariate)==1, yes = prettyNum( n[ rlang::are_na( category) ], big.mark = " "), no = NA),
            "n"            = prettyNum(n, big.mark = " " )
          ) %>% ungroup() %>%


    transmute(     group,  covariate, category,            n,             pct,         cumpct,         cumsum,         subtotal,         n_missing )  %>%
    # selecting the columns
    select_if(    c(TRUE, TRUE      , TRUE    ,    include_n, include_percent, include_cumpct, include_cumsum, include_subtotal, include_n_missing ))  %>%

    plyr::dlply( .variables = "group", function(x) {

      names(x)[4:length(names(x))]   <-  paste0( x$group[1], "_", names(x)[4:length(names(x))] )            # The first 3 names group, covariate, category are not changed.
      x <- x[,-1]
     }   ) %>%

     Reduce( function(x, y) left_join(x, y, by = c("covariate", "category" )), . ) -> count_df_piece

     if(include_p_value) {
       count_df_piece$pvalue <- NA
       count_df_piece$pvalue[1] <- chi_test$p.value
     }

     count_df_piece
}



  chi_test$var_vector          <- var_vector_char_element

freq_fun_total <- function(dataset, var_vector_char_element) {
char_element_symbol  <-  rlang::sym(var_vector_char_element)
by_group_vector          <-  dataset               %>% pull(UQ(by_group_symbol))
var_vector_element_pull  <-  dataset               %>% pull(UQ(var_vector_char_element))
# test
  chi_test               <- chisq.test( x = by_group_vector, y = var_vector_element_pull  ) %>% tidy()
  chi_test$var_vector    <- var_vector_char_element

# count

  dataset %>% count(UQ(by_group_symbol), UQ(char_element_symbol)) %>% complete( UQ(by_group_symbol), UQ(char_element_symbol), fill = list( n = 0 )) %>%

   group_by(UQ(by_group_symbol)) %>%

    mutate( "group" = UQ(by_group_symbol)[1],
            "covariate"    = var_vector_char_element,
            "category"     = UQ(char_element_symbol),
            "pct"          = paste0( round( 100 * n / sum(n), decimal_percent), "%" ),
            "cumpct"       = paste0( round( 100 * cumsum(n) / sum(n), decimal_percent), "%" ),
            "cumsum"       = prettyNum(cumsum(n), big.mark = " " ),
            "subtotal"     = ifelse(  dplyr::row_number(covariate)==1, yes = prettyNum( sum(n), big.mark = " "), no = NA),
            "n_missing"    = ifelse(  dplyr::row_number(covariate)==1, yes = prettyNum( n[ rlang::are_na( category) ], big.mark = " "), no = NA),
            "n"            = prettyNum(n, big.mark = " " )
          ) %>% ungroup() %>%


    transmute(     group,  covariate, category,            n,             pct,         cumpct,         cumsum,         subtotal,         n_missing )  %>%
    # selecting the columns
    select_if(    c(TRUE, TRUE      , TRUE    ,    include_n, include_percent, include_cumpct, include_cumsum, include_subtotal, include_n_missing ))  %>%


# Map freq_fun_total ------------------------------------------------------




# Map freq_fun_by ---------------------------------------------------------

suppressWarnings(
var_vector_char %>%
  purrr::map( .f = function(x) freq_fun_by(dataset = dataset, var_vector_char_element = x ) ) %>%  bind_rows()  )

}

diamonds %>% names() -> testvektor
testvektor <- testvektor[1:9]
freq_by(diamonds, var_vector = testvektor,  by_group = "group_var", include_cumpct = FALSE, include_p_value = TRUE )
freq_by(diamonds, var_vector = var_vector,  by_group = "group_var", include_cumpct = FALSE, include_p_value = TRUE, include_subtotal = TRUE )


