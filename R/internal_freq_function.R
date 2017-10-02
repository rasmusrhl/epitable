#' @title  Frequencies and percentages
#' @description \code{freq_function} creates frequency and percentage tables.
#'
#' @import tidyverse rlang
#' @importFrom dplyr "%>%"
#' @param dataset A dataset
#' @param var_vector For internal use. A character vector containing names of the columns in the
#' input dataset to calculate frequencies and percentages for.
#' @keywords internal


freq_function <- function( dataset, var_vektor ) {


  inner_function <- function(dataset, var) {
    var_symbol <-  rlang::sym(var)
    dataset %>%
      dplyr::count( UQ( var_symbol )) %>%  tidyr::complete( UQ(var_symbol), fill = list( n = 0)) %>%
      dplyr::transmute(
        pct = paste0( round( 100 * n / sum(n)), "%" ),
        n = prettyNum( n, big.mark = " "),
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

