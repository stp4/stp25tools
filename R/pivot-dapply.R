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
  X <- prepare_data2(x, ...)
  apply_data <- dapply2(X$data[X$measure.vars], fun)
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
#'
#' @noRd
#'
dapply1 <-
  function (x,
            fun = function(x) as.numeric(x),
            stringsAsFactors = FALSE,
            ...) {
    if (tibble::is_tibble(x))
      tibble::as_tibble(
        plyr::llply(x, fun, ...))
    else
      data.frame(
        plyr::llply(x, fun, ...),
        stringsAsFactors=stringsAsFactors)
  }

 
 
#' @rdname Dapply
#' 
#' @description 
#' Scaling and Centering of data.frame
#'
#' @param by data.frame, measure and by
#' @param scale default = 1/2 (aproximativ range between -1 and +1)
#' @param reference for factor which level devault = 1
#' @param digits digits
#'
#' @return same as x
#' @export
#'
#' @examples
#' 
#' #' set.seed(1)
#' dat <- data.frame(
#'   x = round(c(rnorm(5), rnorm(5, 10, 2)), 1),
#'   y = c(12, 13, 10, 14, 10, 26, 25, 31, 28, 20) ,
#'   g =  factor(c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), c(TRUE, FALSE))
#' )
#' 
#' 
#' dat |>
#'   scale_by(x, by = ~ g, scale = 1)  #|>  Tbll_desc(x, y, by =  ~ g)
scale_by <- function(x,
                     ...,
                     by = NULL,
                     scale = .5,
                     reference = 1,
                     digits = 3) {
  measure.vars <-
    sapply(lazyeval::lazy_dots(...), function(x) {
      if (!is.character(x$expr))
        as.character(x[1])
    })
  
  if (!is.null(by))
    by <- x[[all.vars(by)]]
  if (is.factor(by))
    by <- by == levels(by)[reference]
  else if (!is.logical(by))
    stop("by muss ein factor oder ein locical -Objekt sein!\n")
  
  
  if (length(measure.vars) == 0)
    dapply2(
      x,
      fun = function(x) {
        if (is.numeric(x)) {
          center <- mean(x[by], na.rm = TRUE)
          cs <- sd(x[by], na.rm = TRUE) / scale
          round(as.vector(scale(x, center, cs)), digits)
        }
        else
          x
      }
    )
  else
    Dapply(
      x,
      ...,
      fun = function(x) {
        if (is.numeric(x)) {
          center <- mean(x[by], na.rm = TRUE)
          cs <- sd(x[by], na.rm = TRUE) / scale
          round(as.vector(scale(x, center, cs)), digits)
        }
        else
          x
      }
    )
  
}


