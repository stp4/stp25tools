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
#'
#' x <- c(1, 0, 0, 0, 1, 1, 0, 3, 2, 2)
#'
#' x <- data.frame(x = x,
#'                 sex = factor2(
#'                   x,
#'                   male = 1,
#'                   female = 0,
#'                   div = 3,
#'                   other = 2
#'                 ))
#' x
#' levels(x$sex)
#'
#'
#' table(reorder2(x$sex, last = "other"))
#' 
#' 
#' #' lattice::barchart(rev(reorder2(x$sex, last = "other")))
#' 
#' # dat<-as.data.frame(table(x$sex)) 
#' # lattice::barchart( 
#' #   reorder2(Var1, Freq, last="other") ~Freq, 
#' #   dat, 
#' #   origin =0)
#' # 
#' # with(
#' #   dat,
#' #   reorder2(Var1, Freq, last="other"))
#' #'
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


#' @rdname factor2
#'
#' @param X,... an reorder
#' @param decreasing an ordere logical
#' @param last character
#'
#' @export
#'
reorder2 <- function(x,
                     X,
                     ...,
                     decreasing = TRUE,
                     last = NULL) {
  if (missing(X)) {
    x <-
      factor(x, levels(x)[order (table(x), decreasing = decreasing)])
    if (!is.null(last))
      for (ref in last)
        x <- relevel2(x, ref)
  }
  else{
    x <- reorder(x, X, ...)
    if (!is.null(last)) 
      for (ref in last)
        x <- relevel(x, ref)
  }
  x
}

# helper 
# letzter level am ende
# copie von relevel ohne fehlerpruefung
relevel2 <- function(x,
                     last,
                     lev = levels(x))  {
  ref <- match(last, lev)
  nlev <- length(lev)
  factor(x, levels = lev[c(seq_along(lev)[-ref], ref)], exclude = NULL)
}

