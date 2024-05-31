# auto_trans




#' transformation
#'
#' @param x Vector
#' @param treshhold grundlienie
#' @param ... weitere Argumente
#'
#' @return Vector mit attr link, inverse und name
#' @export
#'
#' @examples
#' n<-100
#' x1 = rnorm(100)+5
#' x2 = rlnorm(n, meanlog = 0, sdlog = 1)+1
#' x3 = rpois(n, lambda = 1)+1
#' x4 = rweibull(n, shape = .8, scale = 1)+10
#' x5 = runif(n, min = 0, max = 100)+1
#'
#'
#' auto_trans(x2)
#' #'
#' x.neg <- rbeta(10000, 5, 2)  # Negative Skew
#'
#' x.pos <- rexp(1000, 1) # Positive Skew
#' par(mfrow=c(2,2))
#' hist(x.neg)
#' hist(x.pos)
#' hist(auto_trans(x.neg))
#' hist(auto_trans(x.pos))
#'
auto_trans <- function(x, ...){
  UseMethod("auto_trans")
}



#' @rdname auto_trans
#' @param add spalte hinzufügen oder ersätzen
#' @export
#' @examples
#' 
#' n <- 100
#' 
#' scale_100 <- function(x) {
#'   x <- x - min(x)
#'   round(x / max(x) * 100)
#' }
#' 
#' DF <- data.frame(
#'   group = gl(2, n / 2, labels = c("Control", "Treat")),
#'   x1 = scale_100(rnorm(100) + 5),
#'   x2 = scale_100(rlnorm(n, meanlog = 0, sdlog = 1) + 1),
#'   x3 = scale_100(rpois(n, lambda = 1) + 1),
#'   x4 = scale_100(rweibull(n, shape = .8, scale = 1) + 10),
#'   x5 = scale_100(runif(n, min = 0, max = 100) + 1),
#'   x6 = scale_100(sqrt(rnorm(100) + 5))
#' )
#' 
#' auto_trans(DF)
#' 
auto_trans.data.frame <- function(x, 
                                  add = TRUE, 
                                  ...) {
  data <- x
  items <- which(sapply(x, is.numeric))
  x[items] <- dapply2(x[items], fun = auto_trans)
  trans <-unlist(sapply(x[items], attr, "suffix"))
  trans <- trans[!is.na(trans)]

  if (add) {
    x <- x[names(trans)]
    names(x) <- paste0(names(trans), trans)
    lbl <- paste(get_label(x), unlist(sapply(x, attr, "name")))
    names(lbl) <- names(x)
    x <- set_label(x, lbl)
    cbind(data, x)
  }
  else {
    x
  }
}
 

#' @rdname auto_trans
#' @export
auto_trans.default <-
  function(x,
           treshhold = 1,
           ...) {
    if (!is.null(attr(x, "inverse"))) {
      x <-  do.call(attr(x, "inverse"), list(x))
      attr(x, "link") <-   NULL
      attr(x, "inverse") <- NULL
      attr(x, "name") <-  "Re-trans"
      attr(x, "suffix") <-  ".rev"
      return(x)
    }
    
    
    #  negative skew = left-tailed
    # positive skew = right-tailed
    res <-  e1071_skewness(x, na.rm = TRUE)
    trans <-
      if (res > treshhold) {
        x <- log1p(x)
        attr(x, "link") <-   log1p
        attr(x, "inverse") <- expm1
        attr(x, "name") <-  "positive skew (Log)"
        attr(x, "suffix") <-  ".log"
      }
    else  if (res < (treshhold * (-1))) {
      if (max(x, na.rm = TRUE) > 100 & min(x, na.rm = TRUE) >= 0) {
        x <-  x ^ 2
        attr(x, "link") <-   quadr
        attr(x, "inverse") <- sqrt
        attr(x, "name") <-  "negative skew (Square)"
        attr(x, "suffix") <-  ".qdr"
      }
      else if (max(x, na.rm = TRUE) <= 100 &
               min(x, na.rm = TRUE) >= 0) {
        x <-  log1p_percent(x)
        attr(x, "link") <-    log1p_percent
        attr(x, "inverse") <- expm1_percent
        attr(x, "name") <-   "negative skew (max-Log)"
        attr(x, "suffix") <-   ".log"
        
      } else{
        attr(x, "link") <-   identity
        attr(x, "inverse") <- identity
        attr(x, "name") <-  "negative skew (Identity)"
        attr(x, "suffix") <-  NA
      }
    }
    else{
      attr(x, "link") <-   identity
      attr(x, "inverse") <- identity
      attr(x, "name") <-  "normal (Identity)"
      attr(x, "suffix") <-  NA
    }
    x
  }


quadr <- function(x , power = 2) { x ^ power }


#' negative skew (max-Log)
#'
#' Richtung wird vertauscht.
#' Also bei effect-plot Achse auf hoher Wert zu niedriger Wert. (kann nicht geändert werden)
#'
#' @noRd
log1p_percent <- function(x) { log(101 - x) }


expm1_percent <- function(x) { 101 - (exp(x)) }

 


