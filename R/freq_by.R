#' @title  Frequencies and percentages
#' @description \code{freq_by} creates frequency and percentage tables in HTML.
#'
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
#' freq_by(example_data, c("cut", "color"), "clarity", htmlout = FALSE, min_cell_count = 30)


freq_by <- function(dataset, var_vector, by_group = NULL, include_total = TRUE, min_cell_count = 10, htmlout = TRUE, font_css = "font-family: Times;" ) {

  # the worker founction. Defined here so that it inherits parameters from freq_by.
  freq_function <- function( dataset, var_vektor ) {


    inner_function <- function(dataset, var) {
      var_symbol <-  rlang::sym(var)
      dataset %>%
        dplyr::count( UQ( var_symbol )) %>%  tidyr::complete( UQ(var_symbol), fill = list( n = 0)) %>%
        dplyr::transmute(
          pct = dplyr::if_else( n > min_cell_count, true = paste0( round( 100 * n / sum(n)), "%" ), false = "-", missing = "-" ),
          n   = dplyr::if_else( n > min_cell_count, true = prettyNum( n, big.mark = " "), false = paste0("<",min_cell_count), missing = paste0("<",min_cell_count)),
          variable = var,
          category = UQ( var_symbol ) )  %>%

        dplyr::select(variable, category, n, pct )

    }

    suppressWarnings(
      var_vektor %>%
        purrr::map(  function(x) inner_function(dataset, x) )    %>%
        dplyr::bind_rows()
    )
  }


    df_temp                   <- droplevels(as.data.frame(dataset)) # convert to data.frame for easier extraction of values
    dataset                   <- dplyr::as_tibble( droplevels(dataset) )
    by_group_test             <- df_temp[, by_group]
    by_group_test_levels      <- levels(by_group_test)



    # by_group
    if (!rlang::is_null(by_group)) {
      by_group_symbol <- rlang::sym(by_group)   # Needed for non-standard evaluation inside function.

    # test input
      if (!is.factor(by_group_test)) stop("group_var must be of class factor")



      suppressWarnings(
        # suppress warnings for coercion to character
        dataset %>%
          dplyr::group_by(rlang::UQ(by_group_symbol)) %>%
          dplyr::do("by_group_var" = freq_function(., var_vector))  %>%  tidyr::unnest()  %>%
          dplyr::group_by(rlang::UQ(by_group_symbol)) %>%
          dplyr::do(do_data = (.)) %>% dplyr::select(do_data) %>%  purrr::flatten() %>%

          purrr::map(
            .,
            .f = function(x)  {
              names(x)[names(x) %in% c("pct", "n")] <-
                paste0(names(x)[names(x) %in% c("pct", "n")], "_", x[1, 1])
              x[, -1]
            }
          ) %>%
          Reduce(function(x, y)
            dplyr::full_join(x, y,  by = c( "variable", "category" )), .) -> table1
      )

      if (rlang::is_true(include_total)) {
        dataset %>% freq_function(var_vector)    -> table0
        table0 <- dplyr::rename(table0, "n_total" = n, "pct_total" = pct)
        table1 <- dplyr::full_join(table0, table1, by = c("variable", "category"))
      }
    } else {
      dataset %>%
        freq_function(var_vector)    -> table1

      include_total <- FALSE

    }


    if (htmlout == FALSE) { table1 }   else {
      alignment_vektor <- c("l", rep(x = "r", ncol(table1) - 1))

      # rgroup names andd length
      rle_vektor                    <- rle(table1$variable)
      rgroup_vektor                 <- rle_vektor$values
      rgroup_vektor                 <- stringr::str_to_title(rgroup_vektor)
      n_rgroup_vektor               <- rle_vektor$lengths

    if (!rlang::is_null(by_group)) {

        # cgroup names and lengths
        if (rlang::is_true(include_total)) {
          number_of_summaries_in_table  <- 1 + length(by_group_test_levels)
          c_group_vektor                <- c(" ",  c("Total", by_group_test_levels))

        } else  {
          number_of_summaries_in_table <- length(by_group_test_levels)
          c_group_vektor               <- c(" ",  c(by_group_test_levels))
        }

        n_c_group_vektor <- c(1,  rep(2, number_of_summaries_in_table))
    } else if( rlang::is_null(by_group)){
       number_of_summaries_in_table    <- 1
       c_group_vektor                  <- c(" ", " ")
       n_c_group_vektor                <- c(1, 2)
     }

      table1 <- table1[, -1] # exclude first column, because it is shown in the rgroup.
      names(table1) <-
        c(" ", rep(c("n", "(%)"), number_of_summaries_in_table))

      css_matrix     <-
        matrix(data = "padding-left: 0.5cm; padding-right: 0.5cm;",
               nrow = nrow(table1),
               ncol = ncol(table1))
      css_matrix[, 1] <-
        "padding-left: 0.4cm; padding-right: 0.3cm;"

      htmlTable::htmlTable(
        x =  table1,
        rnames     = FALSE,
        cgroup     = c_group_vektor,
        n.cgroup   = n_c_group_vektor,
        rgroup     = rgroup_vektor,
        n.rgroup   = n_rgroup_vektor,
        align      = alignment_vektor,
        css.table  = font_css,
        css.rgroup = "font-style: italic;padding-top: 0.4cm;padding-right: 0.4cm;padding-bottom: 0.2cm;",
        css.cell   = css_matrix
      )

    }
  }


