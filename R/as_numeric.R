#' @name as_irgenwas
#' @title as_numeric and as_logical
#' 
#' @param x vector
#' @param ... weitere methoden
#' @return vector
#'
NULL


#' @rdname as_irgenwas
#' @description as_numeric: character, factor to numeric
#' @param na.string missing
#' @param dec  decimal
#'
#' @export
#'
#' @examples
#'
#' dummy <- factor(c("3", "4", "6"))
#' as_numeric(dummy)
#'
#'  x <-
#' c(
#'   "> 15100",
#'   "+1",
#'   "-1",
#'   "655.39554148943",
#'   "699.913201822519 ",
#'   " 228.40319734460499",
#'   "",
#'   NA,
#'   "hallo1",
#'   "-77"
#' )
#' as_numeric(x,  na.string = c("", "-77"))
#  as_numeric(factor(x))
as_numeric <-  function (x, ...) {
  UseMethod("as_numeric")
}

#' @rdname as_irgenwas
#' @export
as_numeric.numeric <- function(x, ...)
  return(x)



#' @rdname as_irgenwas
#' @description as_numeric: beim Type character wird die Funktion readr::parse_number ausgeführt.
#' parse_number(x, na = c(".", ","), trim_ws=TRUE)
#' 
#' 
#' @param trim_ws  logical trim ws
#' @param ...  readr::parse_number trim_ws	
#' Should leading and trailing whitespace (ASCII spaces and tabs) be trimmed from each field before parsing it?
#' @export
as_numeric.character <-
  function(x,
           na.string = "",
           dec = c(".", ","),
           trim_ws= TRUE,
           ...) {
    lbl <- attr(x, "label")
    if(  length(dec) == 2 & "," %in% dec) { 
      x <- gsub(",", ".", x)
      x <- readr::parse_number(x, 
                               na=na.string, 
                               trim_ws=trim_ws)
      }
    else if( dec[1] == ","){
      x <- readr::parse_number(x, 
                               locale = readr::locale(decimal_mark = ","),
                               na=na.string, 
                               trim_ws=trim_ws)
    }
    else{
      x <- readr::parse_number(x, 
                               na=na.string, 
                               trim_ws=trim_ws)
    }
    
    attr(x, "label") <- lbl
    x
  }

#' @rdname as_irgenwas
#' @description as_numeric: beim Type factor wird zu erst versucht aus den labels  die Zahlen zu extrahieren 
#' und erst wenn das nicht geht die levels (1:n).
#' @export
as_numeric.factor <-   function(x,
                                na.string = "",
                                dec = c(".", ","),
                                ...) {
  lbl <- attr(x, "label")
  #   bei langen Vectoren sollte das schneller sein
  lvl <- as_numeric.character(levels(x), na.string, dec)
  if (!all(is.na(lvl))) {
    levels(x) <- lvl
    x <- as.character(x)
  }
  x <- as.numeric(x)
  attr(x, "label") <- lbl
  x
}


#' @rdname as_irgenwas
#' @description as_logical: alles mit zwei Merkmalen zu logical
#' @export
as_logical <- function(x) {
  lbl <- attr(x, "label")
  if(is.numeric(x)) x == 1
  else if (nlevels(x) == 2)
    x <- x == levels(x)[1]
  else stop("Die Funktion  as_logical kann nur mit zwei Levels arbeiten.")
  attr(x, "label") <- lbl
  x
}







# das geht jetzt mit base::rev 
# (ff <- factor(substring("statistics", 1:10, 1:10), levels = letters))
# attr(ff, "label") <- "lbl:Hallo Welt"
# ff
# base::rev(ff)
# @rdname as_irgenwas
# @description as_rev: reverse factor
# @export
# rev.factor <- as_rev <- function(x) {
#   lbl <- attr(x, "label")
#   x <- factor(x, rev(levels(x)))
#   attr(x, "label") <- lbl
#   x
# }



