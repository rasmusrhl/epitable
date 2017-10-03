#' @title  Frequencies and percentages
#' @description \code{freq_function} creates frequency and percentage tables.
#'
#' @import tidyverse rlang
#' @importFrom dplyr "%>%"
#' @param dataset A dataset
#' @param var_vector For internal use. A character vector containing names of the columns in the
#' input dataset to calculate frequencies and percentages for.
#' @keywords internal
#' ## SET TO BUILD IGNORE - BECAUSE IT IS NOW AN FUNCTION INSIDE FREQ_BY so it
#' ## can inherit from freq_by.



freq_function <- function( dataset, var_vektor ) {


  inner_function <- function(dataset, var) {
    var_symbol <-  rlang::sym(var)
    dataset %>%
      dplyr::count( UQ( var_symbol )) %>%  tidyr::complete( UQ(var_symbol), fill = list( n = 0)) %>%
      dplyr::transmute(
        pct = dplyr::if_else( n >= min_cell_count, true = paste0( round( 100 * n / sum(n)), "%" ), false = "-", missing = "-" ),
        n   = dplyr::if_else( n >= min_cell_count, true = prettyNum( n, big.mark = " "), false = paste0("<=",min_cell_count), missing = paste0("<=",min_cell_count)),
        variable = var,
        category = UQ( var_symbol ) )  %>%

      dplyr::select(variable, category, n, pct )

  }

suppressWarnings( var_vektor %>%
    purrr::map(  function(x) inner_function(dataset, x) )    %>%
    dplyr::bind_rows()
                 )
}

