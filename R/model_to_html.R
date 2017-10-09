#' @title Print regression models to HTML
#'
#' @description Prints on or more lists of regression models to HTML. Currently
#'     only works with class coxph.
#' @param univariate_models_list List of univariate models
#' @param multivariate_models_list List of univariate models
#' @param decimals_estimate A number specifying decimals on estimates. Default is 2.
#' @param font_css A string of CSS code defining the font used for the table.
#'     Default is 'font-family: Times;'. An example of another option is:
#'     'font-family: "Courier New";' Only fonts supported by the browser are
#'     supported.
#' @param exponentiate logical. Sent to broom::tidy(). Defaults to FALSE.
#'     Set to TRUE to exponentiate coefficients and CI of model summary.
#' @param cgroup_names A character vector of length equal to 1 + length of
#'     \code{multivariate_models_list} (if one is supplied).
#' @param html_output logical. Defaults to TRUE. Set to false to output data.frame.
#' @importFrom Hmisc cut2
#' @importFrom dplyr "%>%"
#' @import survival
#' @export model_to_html
#' @examples
#' df         <- survival::lung
#' df$age_bin <- Hmisc::cut2( df$age, g = 5)
#' df$ph_bin  <- Hmisc::cut2( df$ph.karno, g = 5)
#' df$sex     <- factor( df$sex)
#' model1     <- survival::coxph( survival::Surv( time , status==1) ~ ph_bin + wt.loss, data = df)
#'
#' diamonds         <- example_data
#' diamonds$color   <- factor(diamonds$color, ordered = FALSE)
#' diamonds$clarity <- factor(diamonds$clarity, ordered = FALSE)
#' glm_logistic     <- glm( cut=="Ideal" ~  color + clarity + x , data = diamonds, family = "binomial")
#' glm_linear       <- glm( Sepal.Width ~  Petal.Width + Species, data = iris)

model_to_html <- function( univariate_models_list,
                           multivariate_models_list = NULL,
                           decimals_estimate = 2,
                           exponentiate = FALSE,
                           cgroup_names = NULL,
                           html_output  = TRUE,
                           font_css = "font-family: Times;"  ) {


# Check input -------------------------------------------------------------

  if(  ! "list" %in% class(univariate_models_list) ) { # input must be list dont know why
    univariate_models_list <- list(univariate_models_list)
  }

  # if(model_class == "coxph") {
  #   purrr::map( univariate_models_list, class ) %>%
  #    purrr::map( function(x) "coxph" %in% x ) %>%
  #    unlist() %>%
  #    all() -> are_all_models_coxph
  #  if(! are_all_models_coxph ) stop ("When model_class is 'coxph' all models in univariate_models_list must be class 'coxph' ")
  # }

# Internal functions are defined ------------------------------------------

  model_gets_ref_levels        <- function( model_object      ) {

    if ("ordered" %in% attr(model_object$terms, "dataClasses")) {
       stop ("add_reference_levels() does not support ordered factors in the model object. Check the class of the covariates in the model and ensure that they are not class 'ordered' ")
    }


    # extract pretty categorical variables (used for presentation, including ref category)
    cat_variables_n_l      <-  model_object$xlevels
    cat_variables_n        <-  names( cat_variables_n_l )
    cat_variables_l        <-  purrr::map_dbl(cat_variables_n_l,   length )
    cat_variables_output   <-  purrr::map2( cat_variables_n, cat_variables_l, .f = function(x,y ) rep( x, each = y)) %>% unlist()


    # extract pretty categorical categories (used for presentation)
    cat_categories         <- model_object$xlevels %>% unlist() %>% as.character()

    # combine pretty categorical variables and numeric variables

    term_column_numeric    <- names( attr( model_object$terms, "dataClasses" )[
                                   attr( model_object$terms, "dataClasses" )  == "numeric"  ]  )

    # This lines removes any term which is not in coefficients. Otherwise the outcome
    # variable may be included in the list of covariates.
    term_column_numeric    <- term_column_numeric[ term_column_numeric %in% names(model_object$coefficients) ]

    pretty_variables       <- c( cat_variables_output, term_column_numeric )
    pretty_categories      <- c( cat_categories, term_column_numeric )
    column_type            <- c( rep( "char_or_factor", length(cat_variables_output ) ),
                                 rep( "numeric"       , length( term_column_numeric ) ) )

    # create terms column in style of model output (used for join)

    term_column_categoric  <- purrr::map2( cat_variables_n, cat_variables_n_l, function(x,y) paste0(x,y )  )  %>% unlist()


    left_column            <-  data.frame( term = c( term_column_categoric, term_column_numeric ) )
    left_column$variables  <-  pretty_variables
    left_column$categories <-  pretty_categories
    left_column$type       <-  column_type
    left_column$class      <-  paste0( class(model_object), sep = "", collapse = " " )
    left_column$n          <-  if ( "coxph" %in% class(model_object)) model_object$n else {
                                if ( "glm" %in% class(model_object)) nrow( model_object$model) }

    if ( "coxph" %in% class(model_object)) {
    left_column$estimate_name  <- "HR"
    } else if ("binomial" %in%  model_object$family ) {
    left_column$estimate_name  <- "OR"
    } else {
    left_column$estimate_name  <- "&beta;"
    }

    # the full covariate list is left joined with the statistical values
    if (  "glm" %in% class(model_object)) {
     model_with_ref <- broom::tidy( model_object, exponentiate = exponentiate, conf.int = TRUE)
    } else if ( "coxph" %in% class(model_object)) {
     model_with_ref <- broom::tidy( model_object, exponentiate = exponentiate )
    } else {
     model_with_ref <- broom::tidy( model_object ); print("Model class not detected")
    }

    model_with_ref$term     <- stringr::str_replace_all( model_with_ref$term, pattern = "`", replacement = "" ) # Character backtick "`" is removed so join is possible. (feels like a hack :/ )

    suppressWarnings( dplyr::left_join( left_column, model_with_ref, "term" ) ) -> model_with_ref
    model_with_ref

  }
  model_gets_formatted_numbers <- function( output_from_ref_fun ) {


    output_from_ref_fun %>%
      dplyr::transmute( variables,
                 categories,
                 estimate = format( round( estimate, decimals_estimate ), nsmall = decimals_estimate),
                 CI       = paste0( "[",
                                    stringr::str_pad( string = as.character(
                                        format( round( conf.low, decimals_estimate ), nsmall=2)), width = 5, side = "left", pad = " " ),
                                    ",",
                                    stringr::str_pad( string = as.character(
                                        format( round( conf.high, decimals_estimate ),nsmall=2)), width = 5, side = "left" ),
                                    "]"
                                  )
                      )  -> tidy_model

    if (output_from_ref_fun$estimate_name[1] %in% c("HR", "OR")) {
    tidy_model$estimate[ stringr::str_detect(  string = tidy_model$estimate,  pattern =  "NA") ] <- "1"
    } else {
    tidy_model$estimate[ stringr::str_detect(  string = tidy_model$estimate,  pattern =  "NA") ] <- "Ref"
    }

    tidy_model$CI[ stringr::str_detect(  string = tidy_model$CI,  pattern =  "NA") ]             <- "Ref"



    names(tidy_model)[ names(tidy_model)=="estimate" ]  <- output_from_ref_fun$estimate_name[1]

    tidy_model
  }

  model_becomes_html           <- function( tidy_model        ) {


    # tidy_model <- htmloutput # delete me
    # cgroup_names <- " " # delete me
    # n_cgroup     <- 3 # delete me
    # font_css     <- "font-family: monospace;" # delete me

    rgroup_vector       <-   stringr::str_to_title( rle(tidy_model$variables)$values )
    n_rgroup_vector     <-   rle(tidy_model$variables)$lengths
    rgroup_vector[ n_rgroup_vector == 1 ]   <-  "&nbsp;" # single rows dont need rgroup header

    css_rgroup      <- "font-style: italic;padding-top: 0.4cm;padding-right: 0.1cm;padding-bottom: 0.01cm;"
    tidy_model      <- tidy_model[,-1]
    css_matrix      <- matrix(data = "padding-left: 0.3cm; padding-right: 0.3cm;",
                              nrow = nrow(tidy_model),
                              ncol = ncol(tidy_model))
    css_matrix[, 1] <- "padding-left: 0.3cm; padding-right: 0.3cm;"

    names(tidy_model)[  names(tidy_model)=="categories"] <- "&nbsp;"
    names(tidy_model) <- stringr::str_replace( names(tidy_model), "\\..*", "" )


     htmlTable::htmlTable(
     x          = tidy_model ,
     rnames     = FALSE,
     rgroup     = rgroup_vector,
     n.rgroup   = n_rgroup_vector,
     align      = c("l","r"),
     css.rgroup = css_rgroup,
     css.cell   = css_matrix,
     css.table  = font_css,
     cgroup     = cgroup_names,
     n.cgroup   = n_cgroup_names
    ) -> output_htmltable
         output_htmltable
  }

# combine functions -------------------------------------------------------



  # if multivariate_models_list is not provided ------------
  if (rlang::is_null(multivariate_models_list)) {
    if (rlang::is_null(cgroup_names)) {
      cgroup_names <- " "
      n_cgroup_names <- 3
    } else {
      n_cgroup_names <- 3
    }

    univariate_models_list %>%
      purrr::map(model_gets_ref_levels) %>%
      purrr::map(model_gets_formatted_numbers) %>% dplyr::bind_rows() -> univariate_model_output
    # output to either html or dataframe:
    if (rlang::is_true(html_output)) {
      univariate_model_output %>% model_becomes_html()
    } else {
      univariate_model_output
    }

  # if mulativariate_models_list is provided
  } else if (rlang::is_list(multivariate_models_list)) {
    univariate_models_list %>%
      purrr::map(model_gets_ref_levels) %>%
      purrr::map(model_gets_formatted_numbers) %>% dplyr::bind_rows()                      -> left_column

    multivariate_models_list %>%
      purrr::map(model_gets_ref_levels)  %>%
      purrr::map(model_gets_formatted_numbers)  %>%
      Reduce(function(x, y) dplyr::full_join(x, y,  by = c("variables", "categories")), .) -> right_column

    dplyr::left_join(left_column, right_column, by = c("variables", "categories")) -> final_model_as_data_frame

    names(final_model_as_data_frame) <- stringr::str_replace(names(final_model_as_data_frame), "\\..*", "")

    if (rlang::is_null(cgroup_names)) {
      cgroup_names    <-
        c(" ", "Univariate models", paste0("Model ", 1:((
          ncol(final_model_as_data_frame) - 4
        ) / 2)))
      n_cgroup_names  <-
        c(1, rep(2, times = length(cgroup_names) - 1))
    } else if (!rlang::is_null(cgroup_names)) {
      n_cgroup_names  <- c(1, rep(2, times = length(cgroup_names) - 1))
    }


    # output to either HTML or dataframe
    if (!html_output) {
      final_model_as_data_frame
    } else {
      final_model_as_data_frame %>% model_becomes_html()
    }
  }

  # html_table_output[[1]]

}





