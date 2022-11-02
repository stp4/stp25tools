# auc_trapezoid



#' Area Under the Curve
#'
#' Method trapezoid
#'
#' @param ...  value 
#' @param time time 
#' @param na.rm  na.rm
#'
#' @return vector
#' @export
#'
#' @examples
#' 
#'  dat <-
#' data.frame(
#'   t0 = c(1, 1, 1),
#'   t2 = c(1, 2, 3),
#'   t3 = c(1, 3, 5),
#'   t4 = c(1, 3, 6)
#' )
#' 
#' #apply(dat, 1, function(x)
#'  # sum(diff(time) * zoo::rollmean(x, 2)))
#' time = c(0, 1, 2, 3)
#' auc_trapezoid(1,1,1,1)
#' auc_trapezoid(dat )
#' transform(dat, auc= auc_trapezoid( t0, t2, t3, t4, time= time))
#' 
auc_trapezoid <- function(...,
                          time = NULL,
                          na.rm = FALSE) {
  x <- list(...)
  
  if (is.data.frame(x[[1]])) {
    x <-  as.matrix(x[[1]])
  }
  else{
    x <- matrix(unlist(x), ncol = length(x))
  }
  
  if (is.null(time)) {
    time <- seq_len(ncol(x))
    warning("time is missing I use : ", paste(time, collapse = ", "), "\n")
  }
  
  apply(x, 1, function(x1)
    calc_auc_trapezoid(x1, time, na.rm))
}


# stolen from  DescTools::AUC
calc_auc_trapezoid <-
  function (x, 
            time = seq_along(x),
            na.rm = FALSE){
    if(all(is.na(x))) {
    #  print(x)
      return(NA)
      }
    
    if (na.rm) {
      idtime <- na.omit(cbind(time, x))
      time <- time[idtime]
      x <- x[idtime]
    }
    if (length(time) != length(x))
      stop("length time must equal length x")
    idtime <- order(time)
    time <- time[idtime]
    x <- x[idtime]
    sum((apply(cbind(x[-length(x)], x[-1]), 1, mean)) *  (time[-1] - time[-length(time)]))
  }






