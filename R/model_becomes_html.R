#' @title Model becomes HTML
#'
#' @description Takes as input the output from \code{model_gets_formatted_numbers}.
#' @param tidy_model A dataframe.
#' @keywords internal



  model_becomes_html <- function(tidy_model) {

    rgroup_vector       <-   stringr::str_to_title( rle(tidy_model$variables)$values )
    n_rgroup_vector     <-   rle(tidy_model$variables)$lengths
    rgroup_vector[ n_rgroup_vector == 1 ]   <- "&nbsp;" # single rows dont need rgroup header

    css_rgroup      <- "font-style: italic;padding-top: 0.4cm;padding-right: 0.4cm;padding-bottom: 0.2cm;"
    tidy_model      <- tidy_model[,-1]
    css_matrix      <- matrix(data = "padding-left: 0.5cm; padding-right: 0.5cm;",
                              nrow = nrow(tidy_model),
                              ncol = ncol(tidy_model))
    css_matrix[, 1] <- "padding-left: 0.4cm; padding-right: 0.3cm;"

    htmlTable::htmlTable(
     x          = tidy_model ,
     rnames     = FALSE,
     rgroup     = rgroup_vector,
     n.rgroup   = n_rgroup_vector,
     align      = c("l","r"),
     css.rgroup = css_rgroup,
     css.cell   = css_matrix
    )
  }

