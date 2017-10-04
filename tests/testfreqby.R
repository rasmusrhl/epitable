

library(tidyverse)

df <- ggplot2::diamonds

freq_by( c("cut", "color"), "clarity", dataset = ggplot2::diamonds, htmlout = FALSE)

df
c("cut", "color", "price") %>% walk() %>% print()
  map( function(x)  df[, x] )

numeric_data      <- df[, c("cut", "color", "price")] %>% dplyr::select_if( is.numeric )
categorical_data  <- df[, c("cut", "color", "price")] %>% dplyr::select_if( function(x) is.factor(x) | is.character(x) )
c
library(magrittr)

mtcars  %>%  %T>% head()  %>% mutate( "am2" = 2)

rnorm(200) %>%
  matrix(ncol = 2) %T>% print() %>%
  plot %>% # plot usually does not return anything.
  colSums()
c

# before ------------------------------------------------------------------


freq_by <- function(dataset, var_vector, by_group = NULL, include_total = TRUE, min_cell_count = 10, htmlout = TRUE, font_css = "font-family: monospace;" ) {

  # the worker founction. Defined here so that it inherits parameters from freq_by.
  freq_function <- function( dataset, var_vektor ) {

dataset <- diamonds
numeric_data      <- dataset[, c("cut", "color", "price", "clarity")] %>% dplyr::select_if( is.numeric )
categorical_data  <- dataset[, c("cut", "color", "price", "clarity")] %>% dplyr::select_if( function(x) is.factor(x) | is.character(x) )
min_cell_count <- 5
var <- "cut"

    inner_function <- function(grouped_cat_data, var) {
      var_symbol <-  rlang::sym(var)
      grouped_cat_data %>%
        dplyr::count( UQ( var_symbol )) %>%  tidyr::complete( UQ(var_symbol), fill = list( n = 0)) %>%
        transmute(
          n        = n,
          variable = var,
          category = UQ( var_symbol ) )  %>%
        dplyr::select(variable, category, n)
    }


  categorical_data  %>% group_by(clarity ) %>%
   do( "counts" =  count(., cut)  )         ->  out

 out %>% unnest()

 out$asdf
chisq.test(diamonds$clarity, diamonds$cut )
library(dplyr)
iris %>%
  group_by(Species) %>%
  do({
    mod <- lm(Sepal.Length ~ Sepal.Width, data = .)
    pred <- predict(mod, newdata = .["Sepal.Width"])
    data.frame(., .["Species"][1])
  })

cut er grupperingsvariablen
color er tællevariablen
freq_fun <- function("variable_to_count") {

 chisq.test( diamonds %>% pull(color), diamonds %>% pull(cut))

 diamonds %>% count(cut, color) %>% complete( cut, color, fill = list( n = 0 ))  %>%

   group_by(cut) %>% transmute( variable = cut,
                                category = color,
                                  pct    = paste0( round(100* n /sum(n)), "%"),
                                   n     = prettyNum(n, big.mark = " " )) %>%

   print( n = 30)

   reshape2::dcast( formula =  color ~  cut ) -> out

 out  %>%  mutate_if(  is.numeric,  function(x)   sum(x) )


   group_by(cut)  %>% do( data = (.) )  %>% map( .f = function(x) x ) %>%

   flatten() %>% map(  .f = function(x) class(x[, "cut"])  )

   map( .f = function(x)  x %>% flatten()  )


   Reduce(function(x, y)
            dplyr::full_join(x, y,  by = c( "variable", "category" )), .) -> table1


 diamonds %>% group_by(cut) %>% do( "countdata" = count(.,color) %>% mutate( "name" = 2)) %>%
 pull(countdata)
 diamonds %>% select(cut) %>% levels()

dplyr::count( UQ( var_symbol )) %>%  tidyr::complete( UQ(var_symbol), fill = list( n = 0)) %>%
 diamonds %>% pull(cut)

  map( .f = function(x) diamonds[  diamonds$"cut"==x, ] %>% count(color) %>% mutate( "name" =  x))


 diamonds %>% group_by(cut, color) %>% count() %>%
   group_by(cut) %>%
   map( .f = function(x) head(x) )


diamonds %>% select(color) %>%
  map( .f = function(x)  count(x)  )


count(  diamonds %>% select(color)  )


   map( cut)

   do( "asdf" = { count(.,color) %>% mutate( "name" = .$cut[1] ) } )

  diamonds %>% group_by(cut) %>%
   do( "asdf" =  count(., color)    )  %>% map( .f = function(x) str(x))


 diamonds %>% group_by(cut) %>% do( "countdata" = count(.,color)  ) %>% ungroup() %>%
   group_by(cut)
  pull(countdata) %>%  ungroup()






 diamonds %>% group_by(cut) %>% count(color) %>% tidyr::spread(value = n)

iris %>%
  do({
     dels skal der laves en chiqsuared a variable_to_count og grupperingsvariablen,
     den laves af variable_to_count, og en global grupperings_variabel, resultatet gemmes.

    Derudover skal der laves en almindelig count og procenter af
   asdf <-   . %>% summary()
   head(.)
  })
}










diamonds %>% select("color", "cut") %>%
   do( "data1" =  tidy( chisq.test(.$color, .$cut)) )  %>% unnest()

diamonds %>% group_by("color","cut") %>%

  do( "results")





out$asdf
  #   suppressWarnings(
  #     var_vektor %>%
  #       purrr::map(  function(x) inner_function(categorical_data, x) )    %>%
  #   )
  # }


    df_temp                   <- droplevels(as.data.frame(dataset)) # convert to data.frame for easier extraction of values
    dataset                   <- dplyr::as_tibble( droplevels(dataset) )
    by_group_test             <- df_temp[, by_group]
    by_group_test_levels      <- levels(by_group_test)



    # by_group
    if (!rlang::is_null(by_group)) {
      by_group_symbol <- rlang::sym(by_group)   # Needed for non-standard evaluation inside function.

    # test input
      if (!is.factor(by_group_test)) stop("group_var must be of class factor")
var_vektor

diamonds
by_group <- "clarity"
by_group_symbol   <- rlang::sym(by_group)
dataset <- categorical_data

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

       table1




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




# after -------------------------------------------------------------------


