#' @title  Add reference levels
#' @description \code{add_reference_levels} Adds reference levels to regression
#'     model object
#'
#' @param model_object A cox.ph object
#' @details Prints summary statistics of cox.ph model using the
#'     \code{broom}-package, and adds a reference level to the
#'     categorical predictors, as is commonly used in epidemiological
#'     publications.
#' @export add_reference_levels
#' @import survival tidyverse rlang
#' @importFrom dplyr "%>%"
#' @examples
#' # Model for which I want reference levels printed in the output.
#' lung <- survival::lung
#' lung$species <- iris$Species[ sample.int( 150, 228, replace = TRUE )]
#' input_to_function <- survival::coxph( survival::Surv( time, status == 2 ) ~ age +
#' sex  + ph.karno + wt.loss + species, data =  lung)
#' add_reference_levels( model_object = input_to_function)



add_reference_levels <- function( model_object ) {

  if (   class( model_object ) == "coxph" ) {

    # extract pretty categorical variables (used for presentation, including ref category)
    cat_variables_n_l      <-  model_object$xlevels
    cat_variables_n        <-  names( cat_variables_n_l )
    cat_variables_l        <-  map_dbl(cat_variables_n_l,   length )
    cat_variables_output   <-  map2( cat_variables_n, cat_variables_l, .f = function(x,y ) rep( x, each = y)) %>% unlist()


    # extract pretty categorical categories (used for presentation)
    cat_categories         <- model_object$xlevels %>% unlist() %>% as.character()

    # combine pretty categorical variables and numeric variables

    term_column_numeric    <- names( attr( model_object$terms, "dataClasses" )[
                                   attr( model_object$terms, "dataClasses" )  == "numeric"  ]  )

    pretty_variables       <- c( cat_variables_output, term_column_numeric )
    pretty_categories      <- c( cat_categories, term_column_numeric )
    column_type            <- c( rep( "char_or_factor", length(cat_variables_output ) ),
                                 rep( "numeric"       , length( term_column_numeric ) ) )

    # create terms column in style of model output (used for join)

    term_column_categoric  <- map2( cat_variables_n, cat_variables_n_l, function(x,y) paste0(x,y )  )  %>% unlist()


    left_column            <-  data.frame( term = c( term_column_categoric, term_column_numeric ) )
    left_column$variables  <-  pretty_variables
    left_column$categories <-  pretty_categories
    left_column$type       <-  column_type


    # the full covariate list is left joined with the statistical values
    tidy_model_output          <- broom::tidy(model_object, exponentiate = TRUE )
    suppressWarnings( dplyr::left_join( left_column, tidy_model_output, "term" ) ) -> add_ref_output
    add_ref_output
  }

}



