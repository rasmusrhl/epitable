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

  suppressWarnings(
  if (   class( model_object ) == "coxph" ) {


    inside_list1 <- model_object$xlevels
    output_list <- list()

    for (i in 1:length(inside_list1) ) {
      output_list[[i]] <-  paste0( names(inside_list1[i]),  as.character( unlist( inside_list1[i] )  )  )
    }
    categorical_covariates <- unlist( output_list)

    numeric_covarites <- names( attr( model_object$terms, "dataClasses" )[
           attr( model_object$terms, "dataClasses" )  == "numeric"  ]  )
    all_covariates92681 <- c( categorical_covariates, numeric_covarites )
    covariate_df_92681 <- data.frame( term = all_covariates92681)

    # the full covariate list is left joined with the statistical values
    values_92681 <- broom::tidy(model_object, exponentiate = TRUE )
    dplyr::left_join( covariate_df_92681, values_92681, "term" ) -> output_df1
    output_df1$xlevels <- c( as.character( unlist( model_object$xlevels ) ),
                             numeric_covarites )
    output_df1
  }
  else { print( "Only works with coxph objects")}  )
}



