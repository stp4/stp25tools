#' Factors
#'
#' Fancy copie der fun factor.
#'
#' @param x a vector of data,
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
#' @param X,... an reorder  x => an atomic vector, usually a factor
#' X	=> a vector of the same length as x, whose subset of values for each unique level of x determines the eventual order of that level.
#' @param decreasing an ordere logical
#' @param last character
#' @param threshold,threshold.na.strings Anzahl an minimalen Nennungen
#'
#' @export
#'
#' @examples
#'
#'  x <-
#' c(
#'   rep(1, 21),rep(2, 120),rep(3, 28),rep(4, 4),rep(5, 56),
#'   rep(6, 2),rep(7, 92),rep(8, 42),rep(9, 74),rep(10, 20)
#' )
#'
#' x <- factor(x, 1:10, letters[1:10])
#' table(x)
#' table(reorder2(x))
#' table(reorder2(x, threshold = 30))
#'
reorder2 <- function(x,
                     X,
                     ...,
                     decreasing = TRUE,
                     last = NULL,
                     threshold = NULL,
                     threshold.na.strings = "Other") {
  if (missing(X)) {
    if (is.null(threshold)) {
      x <-
        factor(x, levels(x)[order(table(x), decreasing = decreasing)])
    }
    else {
      xt <- table(x)
      
      if (is.na(threshold.na.strings)) {
   
        x <-
          factor(x, names(sort(xt[xt > threshold], 
                               decreasing = decreasing)))
      }
      else{
        x.names <- names(sort(xt, decreasing = decreasing))
        lvl <-
          names(sort(xt[xt > threshold], decreasing = decreasing))
        lvl <-
          c(lvl , rep(threshold.na.strings, length(x.names) - length(lvl)))
        
        x <-
          factor(x, x.names, lvl)
        
      }
      
    }
    
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

# x <-
#   c(
#     rep(1, 21),
#     rep(2, 120),
#     rep(3, 28),
#     rep(4, 4),
#     rep(5, 56),
#     rep(6, 2),
#     rep(7, 92),
#     rep(8, 42),
#     rep(9, 74),
#     rep(10, 20),
#     NA
#   )
# 
# x <- factor(x, 1:10, letters[1:10])
# # table(addNA(x, ifany = FALSE))
# # table(x)
# # table(reorder2(x))
# table(reorder2(x, threshold = 30))