#' Factors
#'
#' Fancy copy of the base function factor.
#'
#' @param x a vector of data,
#' @param ... levels and labels  male = 1, female = 0, 'div inter' = 3, other = 2
#' @param levels,labels,exclude,ordered,nmax an die Funktion factor
#' @param add.na should NA be included
#'
#' @return factor
#' @export
#'
#' @examples
#' 
#' x <- c(1, 0, 0, 0, 1, 1, 0, 3, 2, 2)
#' factor2(x)
#' factor2(x, levels = 1:0)
#' factor2(x, labels = c("m", "f", "d"))
#' factor2(x, male = 1, female = 0, 'div inter' = 3, other = 2)
#' factor2(x, 0:3)
#' factor2(x, 0:3, c("m", "f", "d", "o"))

factor2 <- function(x,
                    ...,
                    levels,
                    labels,
                    exclude = NA,
                    ordered = is.ordered(x),
                    nmax = NA,
                    add.na = FALSE) {
  dots <- unlist(list(...))
  lbl <-  attr(x, "label")
  
  if (length(dots) == 0 & missing(levels) & missing(labels)) {
    x <- factor(x,
                exclude = exclude,
                ordered = ordered,
                nmax = nmax)
  } else if (!missing(levels)) {
    x <- factor(
      x,
      levels = levels,
      labels = labels,
      exclude = exclude,
      ordered = ordered,
      nmax = nmax
    )
  } else if (!missing(labels)) {
    if (is.factor(x))
      lvls <-   levels(x)
    else
      lvls <- seq_along(labels)
    x <- factor(
      x,
      levels = lvls,
      labels = labels,
      exclude = exclude,
      ordered = ordered,
      nmax = nmax
    )
  } else if (length(dots) != 0) {
    labels <-  names(dots)
    if (is.null(labels)) {
      print(class(dots))
      if (is.numeric(dots)) {
        labels <- levels <- dots
      }
      else {
        n_halbe <- length(dots) / 2
        if (n_halbe %% 2 != 0) {
          cat("\n Input: ")
          print(dots)
          stop(
            "Entweder explizit die labels und levels angeben oder zwei gleich lange Vektoren uebergeben."
          )
        }
        levels <- dots[seq_len(n_halbe)]
        labels <-  dots[seq_len(n_halbe) + n_halbe]
      }
    }
    else
      levels <- as.vector(dots)
    
    x <-
      factor(
        x,
        levels = levels,
        labels = labels,
        exclude = exclude,
        ordered = ordered,
        nmax = nmax
      )
    
  }
  
  if(add.na) x <- add_NA(x)
  
  attr(x, "label") <- lbl
  x
}
#' @rdname factor2
#' @param na.string replacement for NA
#' @export
add_NA  <- 
  function (x, na.string = "n.a."){
    if (!is.factor(x)) x <- factor(x)
    if (!anyNA(x)) return(x)
    
    label  <-  attr(x, "label")
    ll <- levels(x)
    ll <- c(ll, na.string)
    x <-  factor(x, levels = ll, exclude = NULL)
    x[ which(is.na(x))] <- na.string
    attr(x, "label")  <- label
    x
  }

# factor2 <- function(x,
#                     ...,
#                     levels,
#                     labels,
#                     exclude = NA,
#                     ordered = is.ordered(x),
#                     nmax = NA) {
#   dots <- unlist(list(...))
#    lbl <-  attr(x, "label")
#   if (missing(levels))
#     labels <-  names(dots)
#   if (missing(levels))
#     levels <- as.vector(dots)
#    
#   x <-  
#   factor(
#     x,
#     levels = levels,
#     labels = labels,
#     exclude = exclude,
#     ordered = ordered,
#     nmax = nmax
#   )
#   
#     attr(x, "label") <- lbl
#   x
# }

 
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








#' @rdname factor2
#'
#' @param X an reorder  x => an atomic vector, usually a factor
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
  lbl <-  attr(x, "label")
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
  
  attr(x, "label") <- lbl
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