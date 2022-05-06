# Long




#' @rdname Long
#' @description  Wide  entspricht tidyr::spread()
#'
#' Quelle: https://community.rstudio.com/t/spread-with-multiple-value-columns/5378
#'
#' @export
#'
#' @examples
#' 
#' #  suppressPackageStartupMessages(library(tidyverse))
#'
#' dat <- data.frame(
#'   month = rep(1:3, 2),
#'   student = factor(rep(c("Amy", "Bob"), each = 3)),
#'   A = c(9, 7, 6, 8, 6, 9),
#'   B = c(6, 7, 8, 5, 6, 7)
#' )
#'
#' dat %>% Wide(student,  c(A, B))
#' dat %>% Wide(student,  c("A", "B"))
#' dat[-3] %>% Wide(student,  B)
#' dat  %>% Wide(student ~ month)
#' #dat[-3] %>% reshape2::dcast(month ~ student)
#' dat  %>% Wide(month ~ student, A)
#' dat  %>% Wide(student ~ month, A)
#'
Wide <- function(...) {
  UseMethod("Wide")
}

#' @rdname Long
#' @export
Wide.formula <- function(x,
                         data,
                         value) {
  valueq <- rlang::enquo(value)
  value_names <-
    paste(rlang::quo_get_expr(valueq))
  
  if (value_names[1] == "")
    value_names <- guess_value(data)
  
  x <-
    clean_dots_formula(x, names_data = names(data))
  
  rhs <- all.vars(x[-2])
  
  if (length(value_names) == 1) {
    data <- data[c(all.vars(x), value_names)]
    data[value_names] <-  delet_label(data[value_names])
    tidyr::pivot_wider(data,
                       names_from = rhs,
                       values_from = value_names)
  } else{
    stop("Das geht nicht! Alternative ist Wide(data, key,  c(A, B))")
  }
  
  
}


#' @rdname Long
#' @export
Wide.data.frame <- function(data, key, value) {

  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  
  # Formula Interface -------------------------------------------------------
  value_names <- paste(rlang::quo_get_expr(valueq))
  key_names <-  rlang::quo_get_expr(keyq)
  
  if (rlang::is_formula(key_names)) {
    key_names <- clean_dots_formula(key_names, names_data = names(data))
    rhs <- all.vars(key_names[[3]])
    
    if (value_names[1] == "")
      value_names <- guess_value(data)
    
    if (length(value_names) == 1) {
      data <- data[c(all.vars(key_names), value_names)]
      data[value_names] <-  delet_label(data[value_names])
      return(tidyr::pivot_wider(
                    data,
                    names_from = !!rhs,
                    values_from = !!value_names))
    } else{
      stop("Das geht nicht! Alternative ist Wide(data, key,  c(A, B))")
    }
  }
  
  # spread ------------------------------------------------------------------
  # test length value
  if (length(rlang::quo_get_expr(valueq)) == 1) {
    return(tidyr::pivot_wider(
      data,
      names_from = !!keyq,
      values_from = !!valueq
    ))
  }
  # multi-value -------------------------------------------------------------
  
  
  
  
  ##  Ersätzen von tidyr::gather durch   pivot_longer
  s <- rlang::quos(!!valueq)
  dat_unite <-
    tidyr::unite(tidyr::gather(data, variable, value,!!!s),
                 temp,
                 !!keyq,
                 variable,
                 sep = "_")
  dat_unite$temp <- factor(dat_unite$temp,
                           paste0(
                             stringr::str_split(levels(factor(dat_unite$temp)), "_" , simplify = T)[, 1],
                             "_",
                             value_names[-1]
                           ))
  return(tidyr::pivot_wider(dat_unite, names_from = temp, values_from = value))
}


#reshape2:::guess_value
guess_value <- function (df){
  if ("value" %in% names(df))
    return("value")
  
  last <- names(df)[ncol(df)]
  message("Using ", last, " as value column: use value to override.")
  last
}


