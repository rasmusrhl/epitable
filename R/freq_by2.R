#'
#'
#'
#'
#'
#'
#' The problem is something about what to do when by_group is null. Then automatically, somehow, the marginal analysis should be made. However it says freq_fun_output not found....
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
diamonds$group_var   <- sample( x = c("Group 1", "Group 2"), size = nrow(diamonds), replace = TRUE)
diamonds$cut[100:200] <- NA
var_vector <- c("cut","color", "price" )

# function
freq_by <- function(dataset, var_vector, by_group = NULL, include_total = FALSE, min_cell_count = 0, htmlout = TRUE, font_css = "font-family: monospace;",
                    include_p_value   = FALSE,
                    decimal_percent   = 0,
                    include_n         = TRUE,
                    include_percent   = TRUE,
                    include_cumpct    = FALSE,
                    include_cumsum    = FALSE,
                    include_subtotal  = FALSE,
                    include_n_missing = FALSE
                                                ) {

  # Initial checks and conversion -------------------------------------------

  if (!rlang::is_null(by_group)) {
  by_group_symbol         <- rlang::sym(by_group)
  }
  var_vector_numeric      <- dataset[, var_vector ] %>% dplyr::select_if( is.numeric ) %>% names()
  var_vector_char         <- dataset[, var_vector ] %>% dplyr::select_if( function(x) is.factor(x) | is.character(x) ) %>% names()



  freq_fun_by <- function(dataset, var_vector_char_element) {

    if( ! rlang::is_null(by_group)) {
    char_element_symbol      <-  rlang::sym(var_vector_char_element)
    by_group_vector          <-  dataset               %>% pull(UQ(by_group_symbol))
    var_vector_element_pull  <-  dataset               %>% pull(UQ(var_vector_char_element))
    # test
    chi_test               <- chisq.test( x = by_group_vector, y = var_vector_element_pull  ) %>% tidy()
    chi_test$var_vector    <- var_vector_char_element
    # count

    dataset %>% count(UQ(by_group_symbol), UQ(char_element_symbol)) %>% complete( UQ(by_group_symbol), UQ(char_element_symbol), fill = list( n = 0 )) %>%

      group_by(UQ(by_group_symbol))  %>%


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

      Reduce( function(x, y) left_join(x, y, by = c("covariate", "category" )), . ) -> freq_fun_output

    if(include_p_value) {
      freq_fun_output$pvalue <- NA
      freq_fun_output$pvalue[1] <- chi_test$p.value

    }
    } # closes if( !if_null(by_group) )

  # freq_fun_total


  freq_fun_total <- function(dataset, var_vector_char_element) {

    # count

    dataset %>% count( UQ(char_element_symbol)) %>% complete( UQ(char_element_symbol), fill = list( n = 0 )) %>%

      mutate(
              "covariate"    = var_vector_char_element,
              "category"     = UQ(char_element_symbol),
              "pct"          = paste0( round( 100 * n / sum(n), decimal_percent), "%" ),
              "cumpct"       = paste0( round( 100 * cumsum(n) / sum(n), decimal_percent), "%" ),
              "cumsum"       = prettyNum(cumsum(n), big.mark = " " ),
              "subtotal"     = ifelse(  dplyr::row_number(covariate)==1, yes = prettyNum( sum(n), big.mark = " "), no = NA),
              "n_missing"    = ifelse(  dplyr::row_number(covariate)==1, yes = prettyNum( n[ rlang::are_na( category) ], big.mark = " "), no = NA),
              "n"            = prettyNum(n, big.mark = " " ) ) %>%

      ungroup() %>%


      transmute(      covariate, category,          n,             pct,         cumpct,         cumsum,         subtotal,         n_missing )  %>%
      # selecting the columns
    select_if(    c( TRUE      , TRUE    ,    include_n, include_percent, include_cumpct, include_cumsum, include_subtotal, include_n_missing )) %>%

    ungroup()    ->    freq_fun_total_output


  } # end of freq fun total (defined inside freq_fun_by)



    # apply freq_fun_total
    freq_fun_total(dataset, var_vector_char_element) -> freq_fun_total_output
    names(freq_fun_total_output)       <- paste0(names(freq_fun_total_output), "_total") # paste "_total" to names
    names(freq_fun_total_output)[1:2]  <- c("covariate", "category")

    if (include_total & ! rlang::is_null(by_group)) {
    dplyr::full_join(freq_fun_total_output, freq_fun_output, by = c("covariate", "category") ) -> freq_fun_output
    }
    freq_fun_output



  } # end of freq_fun_by and freq_fun_total


 # application of freq_fun_by ---------------------------------------------------------

     var_vector_char %>%
       purrr::map( .f = function(x) freq_fun_by(dataset, x ) ) %>% dplyr::bind_rows()   -> freq_fun_output

     n_columns <- c( TRUE      , TRUE    ,    include_n, include_percent, include_cumpct, include_cumsum, include_subtotal, include_n_missing )
     n_columns <- c( n_columns, include_p_value )

     freq_fun_output
     freq_fun_output
  }

freq_by(diamonds, c("cut") )


