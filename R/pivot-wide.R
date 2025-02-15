#' @rdname Long
#' 
#' 
#'
#' @param data dataframe
#' @param ...   namen der weiteren 'key' die mit key oder dem erster Teil der Formula zusammen an 
#'  'values_from' uebergeben wird  
#' @param values_fill Fill in missing values default = NA
#' @param names_sep Trennzeichen "_"
#' @param names_vary an tidyr::pivot_wider
#'
#' @return tibble
#' @importFrom crayon bgMagenta
#' @export
#'  
#' @examples
#' 
#' df <- data.frame(
#'   month = rep(month.abb[1:3], 2),
#'   student = rep(c("Amy", "Bob"), each = 3),
#'   A = c(9, 7, 6, 8, 6, 9),
#'   B = c(6, 7, 8, 5, 6, 7),
#'   C = c(1, 3, 6, 3, 4, 7)
#' )
#' 
#' # das Orginal
#' df |> 
#'   tidyr::pivot_wider( 
#'     names_from = student,   
#'     values_from = c(A, B, C) )
#' 
#' # die Kopie
#' df |> Wide(student, A, B, C)
#' 
#' # aber die Formula kann mehr
#' df |> Wide(month ~ student, A , B, C)
#' df |> Wide(month ~ student, A, B)
#' df |> Wide(~ student, A, B) 
Wide <- function(data,
                 key,
                 ...,
                 values_fill = NULL,
                 names_sep = "_",
                 names_vary = "fastest") {
  if (!is.data.frame(data)) {
    stop("Hier bin ich strickt! Erster Parameter muss ein data.frame sein!")
  }
  values_from <-
    as.character(sapply(lazyeval::lazy_dots(...), function(x) {
      as.character(x[1])
    }, simplify = TRUE))
  
  # verschiedene Schreibweisen
  # formula: Wide(data, month ~ student, A , B, C)
  # formula ist eindeutiger!
  # names: Wide(data, student, A , B, C )
  
  keyq <- rlang::enquo(key)
  names_from <-  rlang::quo_get_expr(keyq)
  
  if (rlang::is_formula(names_from)) {
    key_names <-  rlang::quo_get_expr(keyq)
    # Bei Formula kann der Name der values weggelassen werden.
    if (length(values_from) == 0 &
        ncol(data) == (length(key_names) + 1)) {
      values_from <- setdiff(names(data), all.vars(key_names))
    }
    
    data <- data[c(all.vars(key_names), values_from)]
    
    if (length(all.vars(key_names[-2])) != 0) {
      names_from <- all.vars(key_names[-2])
    }
    else{
      names_from <- all.vars(key_names)
      data <- data |>
        dplyr::grouped_df(names_from) |>
        dplyr::mutate(group_id = dplyr::row_number())
    }
  }
  
  
  if (length(values_from) == 0) {
    stop("Die values muessen schon uebergeben werden!")
  }
  
  
  tidyr::pivot_wider(
    data,
    names_from = !!names_from,
    values_from = !!values_from,
    values_fill = values_fill,
    names_sep = names_sep,
    names_vary = names_vary
  )
  
}







# 
# wide_helper <- function(data, key, value) {
#   
#   keyq <- rlang::enquo(key)
#   valueq <- rlang::enquo(value)
#   # Formula Interface 
#   value_names <- paste(rlang::quo_get_expr(valueq))
#   key_names <-  rlang::quo_get_expr(keyq)
#   
#   if (rlang::is_formula(key_names)) {
#     key_names <- clean_dots_formula(key_names, names_data = names(data))
#     
#     if( length(all.vars(key_names)) == 1  )  stop("Das ist ein Fall fuer rbind().")
#     else  rhs <- all.vars(key_names[[3]])
#     
#     if (value_names[1] == "")
#       value_names <- guess_value(data)
#     
#     if (length(value_names) == 1) {
#       data <- data[c(all.vars(key_names), value_names)]
#       data[value_names] <-  delet_label(data[value_names])
#       return(tidyr::pivot_wider(
#         data,
#         names_from = !!rhs,
#         values_from = !!value_names))
#     } else{
#       cat("Input value_names:")
#       print(value_names)
#       stop("Das geht nicht! Alternative ist Wide(data, key,  c(A, B))")
#     }
#   }
#   
# }


# clean_dots_formula <- stp25tools:::clean_dots_formula
# 
# df <- data.frame(month=rep(1:3,2),
#                  student=rep(c("Amy", "Bob"), each=3),
#                  A=c(9, 7, 6, 8, 6, 9),
#                  B=c(6, 7, 8, 5, 6, 7),
#                  C=c(1, 3, 6, 3, 4, 7))
# 
# df |> Wide(student, A, B, C, names_vary = "slowest")
# df |> Wide(student, A, B, C )
# 
# 
# 
# # Wenn nur namen uebergeben werden wird der gesamte data.frame verarbeitet
# df |> Wide(student, A )
# df |> Wide(month ~ student, A)
# df |> Wide(month+ B ~ student, A)
# 
# df |>  select( student, A)
# tidyr::pivot_wider(
#   names_from = student,
#   values_from = A)
 

 
# Wide_save <- function(data,
#                  key,
#                  ...,
#                  values_fill = NULL,
#                  names_sep = "_",
#                  names_vary = "fastest") {
#   
#   values_from <-
#     as.character(
#       sapply(lazyeval::lazy_dots(...),
#              function(x) {as.character(x[1])},
#              simplify = TRUE))
#   
#   
#   # Wide(a ~ b, data, ...)  erlaubt verschiedene Schreibweisen
#   if (rlang::is_formula(data))  {
#     stop("Hier bin ich strickt! Erster Parameter muss ein data.frame sein!")
#     # names_from <- data
#     #data <- key
#   }
#   else{
#     keyq <- rlang::enquo(key)
#     names_from <-  rlang::quo_get_expr(keyq)
#     if (rlang::is_formula(names_from)) {
#       if (length(values_from) > 0) {
#         if (length(values_from) == 1) {
#           return(wide_helper(data,!!key,!!values_from[1]))
#         }
#         else{
#           rslt <- wide_helper(data,!!key,!!values_from[1])
#           
#           for (i in seq_along(values_from[-1])+1) {
#             rslt <- dplyr::bind_rows(rslt,
#                                      wide_helper(data,!!key,!!values_from[i]),
#                                      .id = "which")
#           }
#           rslt$which <-
#             factor(rslt$which, seq_along(values_from), values_from)
#           return(rslt)
#         }
#       } else {
#         values_from <- all.vars(names_from[[2]])
#         names_from <- all.vars(names_from[[3]])
#       }
#     }
#   }
#   
#   if (length(values_from) == 0) {
#     values_from <- guess_value(data)
#   }
#   # print(list(names_from = names_from,
#   #            values_from = values_from))
#   
#   tidyr::pivot_wider(
#     data,
#     names_from = !!names_from,
#     values_from = !!values_from,
#     values_fill = values_fill,
#     names_sep = names_sep,
#     names_vary = names_vary
#   )
#   
# }
#  
#  

 
 
