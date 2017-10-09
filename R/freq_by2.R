#' @title  Frequencies and percentages, and other statistics
#' @description \code{freq_by2} creates frequency and percentage tables in HTML.
#' @param dataset A dataset
#' @param var_vector A character vector containing names of the columns in the
#' \code{dataset} to calculate frequencies and percentages for.
#' @param by_group A string referring to a factor column in the \code{dataset}
#' by which to stratify the calculations.
#' @param include_total whether to include two extra columns of marginal
#' frequencies and percentages, i.e. not stratified by the \code{by_group}.
#' @param font_css A string of CSS code defining the font used for the table.
#'      Default is'font-family: monospace;'.
#' @param htmlout Whether to output to html (default and intended usage), or
#' as r-dataframe.
#' @param min_cell_count a number which defaults to 10. Used to preserve
#'     anonymity in case of sensitive data. In cells with <= 10 observations,
#'     the string "<=10" is printed.
#' @details The output is a table in HTML which can be viewed in a browser or
#' included in a knitr-report.
#' @import tidyverse rlang
#' @importFrom dplyr "%>%"
#' @export freq_by
#' @examples
#' # Outputs HTML:
#' output <-
#' freq_by2(example_data, c("cut", "color"), "clarity", htmlout = FALSE, min_cell_count = 30)


library(tidyverse)
library(rlang)
library(broom)

# test data
diamonds  <- ggplot2::diamonds
diamonds$group_var   <- sample( x = c("Group 1", "Group 2"), size = nrow(diamonds), replace = TRUE)
diamonds$cut[100:200] <- NA
var_vector <- c("cut","color", "price" )

# function

freq_by2 <- function(dataset, var_vector, by_group = NULL, include_total = TRUE, min_cell_count = 10, htmlout = TRUE, font_css = "font-family: monospace;",
                     include_p_value = FALSE,
                     decimal_percent = 0,
                     include_n = TRUE, include_percent = TRUE,
                     include_cumpct = FALSE, include_cumsum = FALSE, include_subtotal = FALSE, include_n_missing = FALSE) {
    
    by_group_symbol         <- rlang::sym(by_group)
    var_vector_numeric      <- dataset[, var_vector ] %>% dplyr::select_if( is.numeric ) %>% names()
    var_vector_char         <- dataset[, var_vector ] %>% dplyr::select_if( function(x) is.factor(x) | is.character(x) ) %>% names()
    min_cell_count <- 5
    
    freq_fun <- function(dataset, var_vector_char_element) {
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
            
            freq_by <- function(dataset, var_vector, by_group = NULL,  min_cell_count = 0, htmlout = TRUE, font_css = "font-family: monospace;", decimal_percent   = 0,
                                include_p_value   = FALSE,
                                include_total     = TRUE,
                                include_n         = TRUE,
                                include_percent   = TRUE,
                                include_cumpct    = FALSE,
                                include_cumsum    = FALSE,
                                include_subtotal  = FALSE,
                                include_n_missing = FALSE
            ) {
                
                # Initial checks and conversion -------------------------------------------
                
                by_group_symbol         <- rlang::sym(by_group)
                var_vector_numeric      <- dataset[, var_vector ] %>% dplyr::select_if( is.numeric ) %>% names()
                var_vector_char         <- dataset[, var_vector ] %>% dplyr::select_if( function(x) is.factor(x) | is.character(x) ) %>% names()
                
                
                
                freq_fun_by <- function(dataset, var_vector_char_element) {
                    
                    
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
                        freq_fun_output$pvalue[1] <- round( chi_test$p.value, 5)
                    }
                    
                    
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
                    
                    if (include_total ) {
                        dplyr::full_join(freq_fun_total_output, freq_fun_output, by = c("covariate", "category") ) -> freq_fun_output
                    }
                    
                    freq_fun_output # is the only object returned from the function
                }# end of freq_fun_by freq_fun_total
                
                var_vector %>%
                    purrr::map(  function(x) freq_fun_by(dataset, x) ) %>% bind_rows() -> table1
                # freq_fun_output
                
                
                
                header_total <- c("n",                   "%", "Cum. n", "Cum. %", "Subtotal" , "n missing")
                header_total <- header_total[ c(include_n, include_percent, include_cumsum, include_cumpct, include_subtotal, include_n_missing ) ]
                
                
                levels_group     <-  dataset               %>% pull(UQ(by_group_symbol))
                levels_group     <-  levels(levels_group)
                
                html_col_header  <- c(" ", header_total, rep( header_total, times = length(levels_group)) )
                if (include_p_value) {
                    html_col_header  <- c( html_col_header, "p-value")
                }
                
                
                c_group    <-  levels_group
                if (include_total) {
                    c_group  <- c(" ", "Total", levels_group) } else {
                        c_group <- c(" ", levels_group )
                    }
                
                if ( include_total ) {
                    c_group_len <- c(1, length(header_total), rep( length(header_total), times = length(levels_group)) )
                } else {
                    c_group_len <- c(1, rep( length(header_total), times = length(levels_group)) )
                }
                
                if (include_p_value) {
                    c_group_len[ length(c_group_len ) ] <-   c_group_len[ length(c_group_len ) ]  + 1
                }
                
                rle_vector <- rle( as.character(table1$covariate) )
                rgroup_vector <- rle_vector$values
                rgroup_vector <- stringr::str_to_title(rgroup_vector)
                n_rgroup_vector <- rle_vector$lengths
                
                
                alignment  <- c("l", rep("r", ncol(table1) - 1 ))
                list(table1, html_col_header, c_group, c_group_len )
                
                table1         <- table1[,-1]
                names(table1)  <- html_col_header
                
                css_matrix     <- matrix(data     = "padding-left: 0.5cm; padding-right: 0.5cm;",
                                         nrow     = nrow(table1),
                                         ncol     = ncol(table1))
                
                css_matrix[, 1]   <- "padding-left: 0.4cm; padding-right: 0.3cm;"
                
                
                
                
                htmlTable::htmlTable( x = table1, rnames = FALSE,
                                      rgroup = rgroup_vector,
                                      n.rgroup = n_rgroup_vector,
                                      cgroup   = c_group,
                                      n.cgroup = c_group_len,
                                      align    = alignment,
                                      css.rgroup = "font-style: italic;padding-top: 0.4cm;padding-right: 0.4cm;padding-bottom: 0.2cm;",
                                      css.cell   = css_matrix
                )
                
                # htmlTable::htmlTable()
            }
        
        freq_by( dataset = diamonds, var_vector = c("cut", "clarity"), by_group = "color",
                 include_total = TRUE, include_cumpct = FALSE, include_p_value = TRUE ) -> output
        write_file(  x = output, path = temp)
        utils::browseURL(temp)
        
        