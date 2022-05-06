# na_approx


#' Replace NA by Interpolation
#'
#' @param x an object.
#' @param ... 	further arguments passed to methods.
#'
#' @return
#' @export
#'
#' @examples
#' 
#' dat <- data.frame(
#'   u = c(NA, 2,  3,  4,  6,  NA, 7, NA),
#'   v = c(NA, NA, NA, NA, 6,  4,  7, 2),
#'   w = c(NA, 2,  NA, 4,  6,  4,  7, 2),
#'   x = c(NA, 2,  3,  7,  6,  4,  7, NA),
#'   y = c(NA, 7,  2,  NA, 1,  9,  3, 4),
#'   z = c( 1, 7,  2,  8,  NA, 9,  3, 4)
#' )
#' 
#' na_approx(dat$z)
#' na_approx(dat)
#' na_approx(as.matrix(dat))
#' 
na_approx <- function(x, ...) {
  UseMethod("na_approx")
}


#'
#' @param fun Interpolation  zoo::na.approx(z)  zoo::na.fill(z, "extend")  or 
#' @param min.length min length
#'
#' @rdname na_approx
#' @export
na_approx.default <-
  function(x,
           fun = function(z) {
             zoo::na.approx(z, na.rm=FALSE)
           },
           min.length = 3L) {
    x.na <- na.omit(x)
    if (length(x.na) < min.length | length(x.na) == 0)
      return(x)
    if (is.na(x[1]))
      x[1] <- x.na[1]
    if (is.na(x[length(x)]))
      x[length(x)] <- x.na[length(x.na)]
    
    do.call("fun", list(x))
  }


#' @rdname na_approx
#' @export
na_approx.data.frame <- function(x, ...) {
as.data.frame(na_approx.matrix(x))
 
}

#' @rdname na_approx
#' @export
na_approx.matrix <- function(x, ...) {
 rslt <-
   t(apply(x, 1, na_approx.default))
 colnames(rslt) <-colnames(x)
 row.names(rslt) <- row.names(x)
 rslt
}
