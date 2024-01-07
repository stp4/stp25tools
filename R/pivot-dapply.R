# Dapply, dapply2


#' Apply Functions Over Data Frames
#'
#' 
#' Dapply, dapply2:  plyr::llply() + Label()
#' 
#' @param x Objekt data.frame, formula
#' @param ... Weitere Argumente an llply oder prepare_data2
#' @return  data.frame
#' @export
#' @examples
#'  
#' df1 <- Label(data.frame(
#'   month = rep(1:3, 2),
#'   student = rep(c("Amy", "Bob"), each = 3),
#'   A = c(9, 7, 6, 8, 6, 9),
#'   B = c(6, 7, 8, 5, 6, 7)
#' ),
#' A = "Deutsch",
#' B = "Mathe")
#' 
#' rs1 <- Dapply(~ A + B,
#'               df1,
#'               cut,
#'               breaks = 3,
#'               labels = c(1:3))
#'               
Dapply <- function(x, ...) {
  UseMethod("Dapply")
}


#' @rdname Dapply
#' @param data  Data.frame
#' @export
Dapply.formula <- function(x,
                           data,
                           fun = function(y) as.numeric(y),
                           stringsAsFactors = FALSE,
                           ...) {
  X <- prepare_data2(x, data)
  
  apply_data <- dapply2(X$data[X$measure.vars], 
                        fun=fun, 
                        stringsAsFactors=stringsAsFactors,
                        ...)
  
  data[, X$measure.vars] <- apply_data
  data
}


#' @rdname Dapply
#' @export
Dapply.data.frame <- function(x,
                              ...,
                              fun = function(y) as.numeric(y),
                              stringsAsFactors = FALSE) {
  cat("\n in Dapply.data.frame\n")
 # print(head(x))
  X <- prepare_data2(x, ...)
  cat("\n input: \n")
  print(X$x[X$measure.vars])
  
  apply_data <- dapply2(X$x[X$measure.vars], fun)
  cat("\n apply_data: \n")
  print(head(  apply_data  ))
  x[, X$measure.vars] <- apply_data
  x
}


#' @rdname Dapply
#' 
#' @description dapply2: Copie of plyr::llply()
#' @param fun   funktion function(x) as.numeric(x)
#' @param stringsAsFactors logical: should character vectors be converted to factors?
#' @export
#' 
dapply2 <- function (x,
                     fun = function(x) as.numeric(x),
                     stringsAsFactors = FALSE,
                     ...) {
  set_label2(
    dapply1(x,
            fun,
            stringsAsFactors,
            ...),
    get_label2(x))
}

 
  
#' @param data an dplyr
#'
#' @param fun an dplyr default = as.numeric
#' @param stringsAsFactors an data.frame default = FALSE
#' @param ...
#'
#' @noRd
#'
dapply1 <-
  function (x,
            fun = function(x) as.numeric(x),
            stringsAsFactors = FALSE,
            ...) {
    
    cat("\nin dapply1:\n")
    cat( "\ndas habe ich bekommen...\n")
    print(head(x))
    cat("\n\n")
    if (tibble::is_tibble(x))
      tibble::as_tibble(
        plyr::llply(x, fun, ...))
    else
      data.frame(
        plyr::llply(x, fun, ...),
        stringsAsFactors=stringsAsFactors)
  }

 