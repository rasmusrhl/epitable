
 if(getRversion() >= "2.15.1")  utils::globalVariables(c(".",
                                                         "n",
                                                         "variable",
                                                          "category",
                                                          "pct",
                                                          "do_data",
                                                          "variables",
                                                          "categories",
                                                          "estimate",
                                                          "conf.low",
                                                          "conf.high",
                                                          "min_cell_count",
                                                          "nn",
                                                         "censored_var",
                                                         "subtotal",
                                                         "n_missing",
                                                         "covariate",
                                                         "group",
                                                         "cumpct"
                                                           ))



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


### problem: Sæt kombinationer med mindre end min_cell_count til NA. Det er ikke nok at erstattet fx 3 med n<=3, for så kan man jo alligevel se at det er tilfældet.


# dataset <- ggplot2::diamonds
# dataset$color[1000:10000] <- NA
# var_vector <- c("clarity", "cut" )
# by_group = "color"
# include_total = TRUE
# min_cell_count = 10
# htmlout = TRUE
# include_percent = TRUE
# include_cumpct = FALSE
# include_cumsum = FALSE
# include_subtotal = FALSE
# include_n_missing = FALSE
# var_vector_char_element <- "clarity"
# decimal_percent <- 0
# include_n <- TRUE
# include_p_value <- FALSE
#
#
#
# freq_by2 <- function(dataset, var_vector, by_group = NULL, include_total = TRUE, min_cell_count = 10, htmlout = TRUE, font_css = "font-family: monospace;",
#                      include_p_value = FALSE,
#                      decimal_percent = 0,
#                      include_n = TRUE, include_percent = TRUE,
#                      include_cumpct = FALSE, include_cumsum = FALSE, include_subtotal = FALSE, include_n_missing = FALSE) {
#
#     by_group_symbol         <- rlang::sym(by_group)
#     var_vector_numeric      <- dataset[, var_vector ] %>% dplyr::select_if( is.numeric ) %>% names()
#     var_vector_char         <- dataset[, var_vector ] %>% dplyr::select_if( function(x) is.factor(x) | is.character(x) ) %>% names()
#
#    # check
#    dataset %>% dplyr::select_if(is.numeric) %>% names()  %>% paste0( collapse = ", ") -> are_numeric
#    all_char_or_factor <- dataset %>% dplyr::select( UQS(rlang::syms(var_vector)) )  %>%  purrr::map( function(x) any(c("character", "factor", "ordered") %in%  class(x)  ) ) %>% as.logical() %>% all()
#    if(! all_char_or_factor  )  stop("var_vector must refer to columns of class character, factor or ordered. The following are not: ", are_numeric)
#
#
#     freq_fun <- function(dataset, var_vector_char_element) {
#         char_element_symbol      <-  rlang::sym(var_vector_char_element)
#         by_group_vector          <-  dataset               %>% pull(UQ(by_group_symbol))
#         var_vector_element_pull  <-  dataset               %>% pull(UQ(var_vector_char_element))
#         # test
#         chi_test               <- chisq.test( x = by_group_vector, y = var_vector_element_pull  ) %>% broom::tidy()
#         chi_test$var_vector    <- var_vector_char_element
#
#         # count
#
#        dataset %>% dplyr::group_by( UQ(by_group_symbol), UQ(char_element_symbol)  )  %>% mutate( nn = n() ) %>%
#        ungroup() %>% mutate( censored_var = replace(UQ(char_element_symbol), nn <= min_cell_count, NA ) ) -> censored_dataset_1
#
#
#         censored_dataset_1 %>% count(UQ(by_group_symbol), UQ(char_element_symbol)) %>% tidyr::complete( UQ(by_group_symbol), UQ(char_element_symbol), fill = list( n = 0 )) %>%
#
#             dplyr::group_by(UQ(by_group_symbol)) %>%
#
#             dplyr::mutate( "group" = UQ(by_group_symbol)[1],
#                     "covariate"    = var_vector_char_element,
#                     "category"     = UQ(char_element_symbol),
#                     "pct"          = paste0( round( 100 * n / sum(n), decimal_percent), "%" ),
#                     "cumpct"       = paste0( round( 100 * cumsum(n) / sum(n), decimal_percent), "%" ),
#                     "cumsum"       = prettyNum(cumsum(n), big.mark = " " ),
#                     "subtotal"     = ifelse(  dplyr::row_number(covariate)==1, yes = prettyNum( sum(n), big.mark = " "), no = NA),
#                     "n_missing"    = ifelse(  dplyr::row_number(covariate)==1, yes = prettyNum( n[ rlang::are_na( category) ], big.mark = " "), no = NA),
#                     "n"            = prettyNum(n, big.mark = " " )
#             ) %>% dplyr::ungroup() %>%
#
#             dplyr::transmute(     group,  covariate, category,            n,             pct,         cumpct,         cumsum,         subtotal,         n_missing )  %>%
#
#                         # selecting the columns
#            dplyr::select_if(    c(TRUE, TRUE      , TRUE    ,    include_n, include_percent, include_cumpct, include_cumsum, include_subtotal, include_n_missing ))  %>%
#
#            plyr::dlply( .variables = "group", function(x) {
#
#                 names(x)[4:length(names(x))]   <-  paste0( x$group[1], "_", names(x)[4:length(names(x))] )            # The first 3 names group, covariate, category are not changed.
#                 x <- x[,-1]
#             }   ) %>%
#
#             Reduce( function(x, y) left_join(x, y, by = c("covariate", "category" )), . ) -> freq_fun_output
#
#                     if(include_p_value) {
#                         freq_fun_output$pvalue <- NA
#                         freq_fun_output$pvalue[1] <- base::format.pval( chi_test$p.value )
#                     }
#
#            # freq_fun_total (defined inside freq_fun_by )
#
#               freq_fun_total <- function(dataset, var_vector_char_element) {
#
#
#                  dataset %>% dplyr::group_by( UQ(char_element_symbol)  )  %>% mutate( nn = n() ) %>%
#                  ungroup() %>% mutate( censored_var = replace(UQ(char_element_symbol), nn <= min_cell_count, NA ) ) -> censored_dataset_2
#
#                   # count
#
#
#                   censored_dataset_2 %>% dplyr::count( UQ(char_element_symbol)) %>% tidyr::complete( rlang::UQ(char_element_symbol), fill = list( n = 0 )) %>%
#
#                       dplyr::mutate(
#                           "covariate"    = var_vector_char_element,
#                           "category"     = UQ(char_element_symbol),
#                           "pct"          = paste0( round( 100 * n / sum(n), decimal_percent), "%" ),
#                           "cumpct"       = paste0( round( 100 * cumsum(n) / sum(n), decimal_percent), "%" ),
#                           "cumsum"       = prettyNum(cumsum(n), big.mark = " " ),
#                           "subtotal"     = ifelse(  dplyr::row_number(covariate)==1, yes = prettyNum( sum(n), big.mark = " "), no = NA),
#                           "n_missing"    = ifelse(  dplyr::row_number(covariate)==1, yes = prettyNum( n[ rlang::are_na( category) ], big.mark = " "), no = NA),
#                           "n"            = prettyNum(n, big.mark = " " ) ) %>%
#
#                       dplyr::ungroup() %>%
#
#                       dplyr::transmute(      covariate, category,          n,             pct,         cumpct,         cumsum,         subtotal,         n_missing )  %>%
#
#                       # selecting the columns
#                       dplyr::select_if(    c( TRUE      , TRUE    ,    include_n, include_percent, include_cumpct, include_cumsum, include_subtotal, include_n_missing )) %>%
#
#                       dplyr::ungroup()    ->    freq_fun_total_output
#
#               } # end of freq fun total (defined inside freq_fun)
#
#                     # apply freq_fun_total
#                     freq_fun_total(dataset, var_vector_char_element) -> freq_fun_total_output
#                     names(freq_fun_total_output)       <- paste0(names(freq_fun_total_output), "_total") # paste "_total" to names
#                     names(freq_fun_total_output)[1:2]  <- c("covariate", "category")
#
#                     if (include_total ) {
#                         dplyr::full_join(freq_fun_total_output, freq_fun_output, by = c("covariate", "category") ) -> freq_fun_output
#                     }
#
#                     freq_fun_output # is the only object returned from the function
#                 }# end of freq_fun freq_fun_total
#                 suppressWarnings(
#                 var_vector %>%
#                     purrr::map(  function(x) freq_fun(dataset, x) ) %>% dplyr::bind_rows() -> table1   )
#                 # freq_fun_output
#
#                 header_total <- c("n",                   "%", "Cum. n", "Cum. %", "Subtotal" , "n missing")
#                 header_total <- header_total[ c(include_n, include_percent, include_cumsum, include_cumpct, include_subtotal, include_n_missing ) ]
#
#
#                 levels_group           <-  dataset               %>% dplyr::pull(rlang::UQ(by_group_symbol))
#                 any_na_in_by_group     <-  dataset               %>% dplyr::pull(rlang::UQ(by_group_symbol)) %>% anyNA()
#
#                 if(any_na_in_by_group){
#                  levels_group <- levels_group %>% levels() %>% as.character()
#                  levels_group <- c(levels_group, "Missing" )
#                 } else {
#                  levels_group <- levels_group %>% levels() %>% as.character()
#                 }
#
#                 html_col_header  <- c(" ", header_total, rep( header_total, times = length(levels_group)) )
#
#
#                 c_group    <-  levels_group
#                 if (include_total) {
#                     c_group  <- c(" ", "Total", levels_group) } else {
#                         c_group <- c(" ", levels_group )
#                     }
#
#                 if ( include_total ) {
#                     c_group_len <- c(1, length(header_total), rep( length(header_total), times = length(levels_group)) )
#                 } else {
#                     c_group_len <- c(1, rep( length(header_total), times = length(levels_group)) )
#                 }
#
#                 if (include_p_value) {
#                   c_group     <- c( c_group, " ")
#                   c_group_len <- c( c_group_len, 1)
#                   html_col_header  <- c( html_col_header, "p-value")
#                 }
#
#                 rle_vector <- rle( as.character(table1$covariate) )
#                 rgroup_vector <- rle_vector$values
#                 rgroup_vector <- stringr::str_to_title(rgroup_vector)
#                 n_rgroup_vector <- rle_vector$lengths
#
#                 alignment  <- c("l", rep("r", ncol(table1) - 1 ))
#                 # list(table1, html_col_header, c_group, c_group_len )
#
#                 table1         <- table1[,-1]
#                 names(table1)  <- html_col_header
#
#                 css_matrix     <- matrix(data     = "padding-left: 0.5cm; padding-right: 0.5cm;",
#                                          nrow     = nrow(table1),
#                                          ncol     = ncol(table1))
#
#                 css_matrix[, 1]   <- "padding-left: 0.4cm; padding-right: 0.3cm;"
#
#                 htmlTable::htmlTable( x = table1, rnames = FALSE,
#                                       rgroup = rgroup_vector,
#                                       n.rgroup = n_rgroup_vector,
#                                       cgroup   = c_group,
#                                       n.cgroup = c_group_len,
#                                       align    = alignment,
#                                       css.rgroup = "font-style: italic;padding-top: 0.4cm;padding-right: 0.4cm;padding-bottom: 0.2cm;",
#                                       css.cell   = css_matrix,
#                                       pos.caption = "top"
#                 )
#
# }
#
#         freq_by2( dataset = diamonds, var_vector = c("clarity", "cut"), by_group = "color",
#                  include_total = TRUE, include_subtotal = FALSE, include_cumpct = FALSE, include_p_value = TRUE ) -> output
#
# temp <-   tempfile( pattern = "asdf", fileext = ".html")
#         readr::write_file(  x = output, path = temp)
#         utils::browseURL(temp)


# freq_by2 <- function(dataset, var_vector, by_group = NULL, include_total = TRUE, min_cell_count = 10, htmlout = TRUE, font_css = "font-family: monospace;",
#                      include_p_value = FALSE,
#                      decimal_percent = 0,
#                      include_n = TRUE, include_percent = TRUE,
#                      include_cumpct = FALSE, include_cumsum = FALSE, include_subtotal = FALSE, include_n_missing = FALSE) {
#
#   by_group_symbol         <- rlang::sym(by_group)
#   var_vector_numeric      <- dataset[, var_vector ] %>% dplyr::select_if( is.numeric ) %>% names()
#   var_vector_char         <- dataset[, var_vector ] %>% dplyr::select_if( function(x) is.factor(x) | is.character(x) ) %>% names()
#
#   # check
#   dataset %>% dplyr::select_if(is.numeric) %>% names()  %>% paste0( collapse = ", ") -> are_numeric
#   all_char_or_factor <- dataset %>% dplyr::select( UQS(rlang::syms(var_vector)) )  %>%  purrr::map( function(x) any(c("character", "factor", "ordered") %in%  class(x)  ) ) %>% as.logical() %>% all()
#   if(! all_char_or_factor  )  stop("var_vector must refer to columns of class character, factor or ordered. The following are not: ", are_numeric)
#
#
#   freq_fun <- function(dataset, var_vector_char_element) {
#     char_element_symbol      <-  rlang::sym(var_vector_char_element)
#     by_group_vector          <-  dataset               %>% pull(UQ(by_group_symbol))
#     var_vector_element_pull  <-  dataset               %>% pull(UQ(var_vector_char_element))
#     # test
#     chi_test               <- chisq.test( x = by_group_vector, y = var_vector_element_pull  ) %>% broom::tidy()
#     chi_test$var_vector    <- var_vector_char_element
#
#     # count
#
#     dataset %>% dplyr::group_by( UQ(by_group_symbol), UQ(char_element_symbol)  )  %>% mutate( nn = n() ) %>%
#       ungroup() %>% mutate( censored_var = replace(UQ(char_element_symbol), nn <= min_cell_count, NA ) ) -> censored_dataset_1
#
#
#     censored_dataset_1 %>% count(UQ(by_group_symbol), censored_var) %>% tidyr::complete( UQ(by_group_symbol), censored_var, fill = list( n = 0 )) %>%
#
#       dplyr::group_by(UQ(by_group_symbol)) %>%
#
#       dplyr::mutate( "group" = UQ(by_group_symbol)[1],
#                      "covariate"    = var_vector_char_element,
#                      "category"     = censored_var,
#                      "pct"          = paste0( round( 100 * n / sum(n), decimal_percent), "%" ),
#                      "cumpct"       = paste0( round( 100 * cumsum(n) / sum(n), decimal_percent), "%" ),
#                      "cumsum"       = prettyNum(cumsum(n), big.mark = " " ),
#                      "subtotal"     = ifelse(  dplyr::row_number(covariate)==1, yes = prettyNum( sum(n), big.mark = " "), no = NA),
#                      "n_missing"    = ifelse(  dplyr::row_number(covariate)==1, yes = prettyNum( n[ rlang::are_na( category) ], big.mark = " "), no = NA),
#                      "n"            = prettyNum(n, big.mark = " " )
#       ) %>% dplyr::ungroup() %>%
#
#       dplyr::transmute(     group,  covariate, category,            n,             pct,         cumpct,         cumsum,         subtotal,         n_missing )  %>%
#
#       # selecting the columns
#       dplyr::select_if(    c(TRUE, TRUE      , TRUE    ,    include_n, include_percent, include_cumpct, include_cumsum, include_subtotal, include_n_missing ))  %>%
#
#       plyr::dlply( .variables = "group", function(x) {
#
#         names(x)[4:length(names(x))]   <-  paste0( x$group[1], "_", names(x)[4:length(names(x))] )            # The first 3 names group, covariate, category are not changed.
#         x <- x[,-1]
#       }   ) %>%
#
#       Reduce( function(x, y) left_join(x, y, by = c("covariate", "category" )), . ) -> freq_fun_output
#
#     if(include_p_value) {
#       freq_fun_output$pvalue <- NA
#       freq_fun_output$pvalue[1] <- base::format.pval( chi_test$p.value )
#     }
#
#     # freq_fun_total (defined inside freq_fun_by )
#
#     freq_fun_total <- function(dataset, var_vector_char_element) {
#
#
#       dataset %>% dplyr::group_by( UQ(char_element_symbol)  )  %>% mutate( nn = n() ) %>%
#         ungroup() %>% mutate( censored_var = replace(UQ(char_element_symbol), nn <= min_cell_count, NA ) ) -> censored_dataset_2
#
#       # count
#
#
#       censored_dataset_2 %>% dplyr::count(censored_var) %>% tidyr::complete( censored_var, fill = list( n = 0 )) %>%
#
#         dplyr::mutate(
#           "covariate"    = var_vector_char_element,
#           "category"     = censored_var,
#           "pct"          = paste0( round( 100 * n / sum(n), decimal_percent), "%" ),
#           "cumpct"       = paste0( round( 100 * cumsum(n) / sum(n), decimal_percent), "%" ),
#           "cumsum"       = prettyNum(cumsum(n), big.mark = " " ),
#           "subtotal"     = ifelse(  dplyr::row_number(covariate)==1, yes = prettyNum( sum(n), big.mark = " "), no = NA),
#           "n_missing"    = ifelse(  dplyr::row_number(covariate)==1, yes = prettyNum( n[ rlang::are_na( category) ], big.mark = " "), no = NA),
#           "n"            = prettyNum(n, big.mark = " " ) ) %>%
#
#         dplyr::ungroup() %>%
#
#         dplyr::transmute(      covariate, category,          n,             pct,         cumpct,         cumsum,         subtotal,         n_missing )  %>%
#
#         # selecting the columns
#         dplyr::select_if(    c( TRUE      , TRUE    ,    include_n, include_percent, include_cumpct, include_cumsum, include_subtotal, include_n_missing )) %>%
#
#         dplyr::ungroup()    ->    freq_fun_total_output
#
#     } # end of freq fun total (defined inside freq_fun)
#
#     # apply freq_fun_total
#     freq_fun_total(dataset, var_vector_char_element) -> freq_fun_total_output
#     names(freq_fun_total_output)       <- paste0(names(freq_fun_total_output), "_total") # paste "_total" to names
#     names(freq_fun_total_output)[1:2]  <- c("covariate", "category")
#
#     if (include_total ) {
#       dplyr::full_join(freq_fun_total_output, freq_fun_output, by = c("covariate", "category") ) -> freq_fun_output
#     }
#
#     freq_fun_output # is the only object returned from the function
#   }# end of freq_fun freq_fun_total
#   suppressWarnings(
#     var_vector %>%
#       purrr::map(  function(x) freq_fun(dataset, x) ) %>% dplyr::bind_rows() -> table1   )
#   # freq_fun_output
#
#   header_total <- c("n",                   "%", "Cum. n", "Cum. %", "Subtotal" , "n missing")
#   header_total <- header_total[ c(include_n, include_percent, include_cumsum, include_cumpct, include_subtotal, include_n_missing ) ]
#
#
#   levels_group           <-  dataset               %>% dplyr::pull(rlang::UQ(by_group_symbol))
#   any_na_in_by_group     <-  dataset               %>% dplyr::pull(rlang::UQ(by_group_symbol)) %>% anyNA()
#
#   if(any_na_in_by_group){
#     levels_group <- levels_group %>% levels() %>% as.character()
#     levels_group <- c(levels_group, "(Missing)" )
#   } else {
#     levels_group <- levels_group %>% levels() %>% as.character()
#   }
#
#   html_col_header  <- c(" ", header_total, rep( header_total, times = length(levels_group)) )
#
#
#   c_group    <-  levels_group
#   if (include_total) {
#     c_group  <- c(" ", "Total", levels_group) } else {
#       c_group <- c(" ", levels_group )
#     }
#
#   if ( include_total ) {
#     c_group_len <- c(1, length(header_total), rep( length(header_total), times = length(levels_group)) )
#   } else {
#     c_group_len <- c(1, rep( length(header_total), times = length(levels_group)) )
#   }
#
#   if (include_p_value) {
#     c_group     <- c( c_group, " ")
#     c_group_len <- c( c_group_len, 1)
#     html_col_header  <- c( html_col_header, "p-value")
#   }
#
#   rle_vector <- rle( as.character(table1$covariate) )
#   rgroup_vector <- rle_vector$values
#   rgroup_vector <- stringr::str_to_title(rgroup_vector)
#   n_rgroup_vector <- rle_vector$lengths
#
#   alignment  <- c("l", rep("r", ncol(table1) - 1 ))
#   # list(table1, html_col_header, c_group, c_group_len )
#
#   table1         <- table1[,-1]
#   names(table1)  <- html_col_header
#
#   css_matrix     <- matrix(data     = "padding-left: 0.5cm; padding-right: 0.5cm;",
#                            nrow     = nrow(table1),
#                            ncol     = ncol(table1))
#
#   css_matrix[, 1]   <- "padding-left: 0.4cm; padding-right: 0.3cm;"
#
#   htmlTable::htmlTable( x = table1, rnames = FALSE,
#                         rgroup = rgroup_vector,
#                         n.rgroup = n_rgroup_vector,
#                         cgroup   = c_group,
#                         n.cgroup = c_group_len,
#                         align    = alignment,
#                         css.rgroup = "font-style: italic;padding-top: 0.4cm;padding-right: 0.4cm;padding-bottom: 0.2cm;",
#                         css.cell   = css_matrix,
#                         pos.caption = "top"
#   )
#
# }
#

