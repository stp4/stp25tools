#' @rdname Long
#'
#' @param data dataframe
#' @param ...  key +  values_fro or erster Teil der Formula
#' @param values_fill,names_sep,names_vary an tidyr::pivot_wider
#'
#' @return tibble
#' @importFrom crayon bgMagenta
#' @export
#' 
Wide <- function(data,
                  key,
                  ...,
                  values_fill = NULL,
                  names_sep = "_",
                  names_vary = "fastest") {
  
 
  cat(crayon::bgMagenta('\n\n   Achtung neue Version von Wide() !!!!\n\n' )  )
  
  values_from <-
    as.character(sapply(lazyeval::lazy_dots(...),
                        function(x) {
                          as.character(x[1])
                        },
                        simplify = TRUE))
  
  # erlaubt verschiedene Schreibweisen
  # Wide(formula, data, ...)
  if (rlang::is_formula(data))  {
    # my.formula <- data
    names_from <- data
    data <- key
  }
  else{
    keyq <- rlang::enquo(key)
    names_from <-  rlang::quo_get_expr(keyq)
    if (rlang::is_formula(names_from)) {
      if (length(values_from) > 0) {
        if (length(values_from) == 1) {
          return(wide_helper(data,!!key,!!values_from[1]))
        }
        else{
          rslt <- wide_helper(data,!!key,!!values_from[1])
          
          for (i in seq_along(values_from[-1])+1) {
            rslt <- dplyr::bind_rows(rslt,
                                     wide_helper(data,!!key,!!values_from[i]),
                                     .id = "which")
          }
          rslt$which <-
            factor(rslt$which, seq_along(values_from), values_from)
          return(rslt)
        }
      } else {
        values_from <- all.vars(names_from[[2]])
        names_from <- all.vars(names_from[[3]])
      }
    }
  }
  
  if (length(values_from) == 0) {
    values_from <- guess_value(data)
  }
  # print(list(names_from = names_from,
  #            values_from = values_from))
  
  tidyr::pivot_wider(
    data,
    names_from = !!names_from,
    values_from = !!values_from,
    values_fill = values_fill,
    names_sep = names_sep,
    names_vary = names_vary
  )
  
}

wide_helper <- function(data, key, value) {
  
  keyq <- rlang::enquo(key)
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
  
}





# Wide <- function(...) {
#   UseMethod("Wide")
# }
# 
# 
# Wide.formula <- function(x,
#                          data,
#                          value) {
#   valueq <- rlang::enquo(value)
#   value_names <-
#     paste(rlang::quo_get_expr(valueq))
#   
#   if (value_names[1] == "")
#     value_names <- guess_value(data)
#   
#   x <-
#     clean_dots_formula(x, names_data = names(data))
#   
#   rhs <- all.vars(x[-2])
#   
#   if (length(value_names) == 1) {
#     data <- data[c(all.vars(x), value_names)]
#     data[value_names] <-  delet_label(data[value_names])
#     tidyr::pivot_wider(data,
#                        names_from = rhs,
#                        values_from = value_names)
#   } else{
#     stop("Das geht nicht! Alternative ist Wide(data, key,  c(A, B))")
#   }
#   
#   
# }



# Wide.data.frame <- function(data, key, value) {
# 
#   keyq <- rlang::enquo(key)
#   # break value vector into quotes
#   valueq <- rlang::enquo(value)
#   
#   # Formula Interface -------------------------------------------------------
#   value_names <- paste(rlang::quo_get_expr(valueq))
#   key_names <-  rlang::quo_get_expr(keyq)
#   
#   if (rlang::is_formula(key_names)) {
#     key_names <- clean_dots_formula(key_names, names_data = names(data))
#     rhs <- all.vars(key_names[[3]])
#     
#     if (value_names[1] == "")
#       value_names <- guess_value(data)
#     
#     if (length(value_names) == 1) {
#       data <- data[c(all.vars(key_names), value_names)]
#       data[value_names] <-  delet_label(data[value_names])
#       return(tidyr::pivot_wider(
#                     data,
#                     names_from = !!rhs,
#                     values_from = !!value_names))
#     } else{
#       stop("Das geht nicht! Alternative ist Wide(data, key,  c(A, B))")
#     }
#   }
#   
#   # spread ------------------------------------------------------------------
#   # test length value
#   if (length(rlang::quo_get_expr(valueq)) == 1) {
#     return(tidyr::pivot_wider(
#       data,
#       names_from = !!keyq,
#       values_from = !!valueq
#     ))
#   }
#   # multi-value -------------------------------------------------------------
#   
#   
#   
#   
#   ##  Ersätzen von tidyr::gather durch   pivot_longer
#   s <- rlang::quos(!!valueq)
#   dat_unite <-
#     tidyr::unite(tidyr::gather(data, variable, value,!!!s),
#                  temp,
#                  !!keyq,
#                  variable,
#                  sep = "_")
#   dat_unite$temp <- factor(dat_unite$temp,
#                            paste0(
#                              stringr::str_split(levels(factor(dat_unite$temp)), "_" , simplify = T)[, 1],
#                              "_",
#                              value_names[-1]
#                            ))
#   return(tidyr::pivot_wider(dat_unite, names_from = temp, values_from = value))
# }





