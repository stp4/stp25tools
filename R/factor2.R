#' Factors
#' 
#' Fancy copie der fun factor.
#'
#' @param x a vector of data,
#' @param ...  levels
#' @param levels,labels,exclude,ordered,nmax an die Funktion factor
 
#' @return factor 
#' @export
#'
#' @examples
#' 
#' factor2(c(1,0,0,0,1,1,0), male = 1, female = 0)
factor2 <- function(x,
                    ...,
                    levels,
                    labels,
                    exclude = NA,
                    ordered = is.ordered(x),
                    nmax = NA) {
  dots <- unlist(list(...))
  if (missing(levels))
    labels <-  names(dots)
  if (missing(levels))
    levels <- as.vector(dots)
  factor(
    x,
    levels = levels,
    labels = labels,
    exclude = exclude,
    ordered = ordered,
    nmax = nmax
  )
}
