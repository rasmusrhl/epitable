#' @title  Frequencies and percentages
#' @description \code{freq_by} creates frequency and percentage tables in HTML.
#'
#' @param dataset A dataset
#' @param var_vector A character vector containing names of the columns in the
#' input dataset to calculate frequencies and percentages for.
#' @param by_group A string referring to a factor column in the input dataset
#' by which to stratify the calculations.
#' @param include_total whether to include frequencies and percentages not
#' stratified by the \code{by_group}.
#' @param htmlout Whether to output to html (default and intended usage), or
#' as r-dataframe.
#' @details The output is a table in HTML which can be viewed in a browser or
#' included in a knitr-report.
#' @import tidyverse rlang
#' @importFrom dplyr "%>%"
#' @export freq_by
#' @examples
#' # Outputs HTML:
#' # output <- freq_by(example_data, c("cut", "color"), "clarity")
#' # temp1 <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".html")
#' # readr::write_file( output, temp1 )
#' # utils::browseURL( temp1 )


freq_by <- function(dataset, var_vector, by_group = NULL, include_total = TRUE, htmlout = TRUE) {

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
        rnames = FALSE,
        cgroup   = c_group_vektor,
        n.cgroup = n_c_group_vektor,
        rgroup   = rgroup_vektor,
        n.rgroup = n_rgroup_vektor,
        align = alignment_vektor,
        css.rgroup = "font-style: italic;padding-top: 0.4cm;padding-right: 0.4cm;padding-bottom: 0.2cm;",
        css.cell = css_matrix
      )

    }
  }


