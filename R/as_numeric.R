#' Nummernindex aus Excel - Spaltenbezeichnung
#'
#' Extrahiert aus Buchstaben die Spaltennummer
#' @param ... liste mit den Spaltennamen A:BB
#' @export
#' @examples
#' as.roman(1968)
#' #strsplit("A:V", "\\:")
#' XLS(a, B)
#' XLS(a, B, c:f, g:h,i, r:z)
#' XLS(A:Z)
#'
XLS <- function(...) {
  letter_num <- function(ltr) {
    which(myLetters %in% ltr)
  }
  
  myLetters = c(LETTERS,
                unlist(lapply(LETTERS, function(abc)
                  paste0(abc, LETTERS))))
  
  ltr <- toupper(as.character(sys.call())[-1])
  
  
  xrange <- grep("\\:", ltr)
  n <- 0
  if (length(xrange)) {
    for (i in seq_along(xrange)) {
      posn <- xrange[i] + n - i + 1
      mltr <- unlist(strsplit(ltr[posn], "\\:"))
      myRange <- myLetters[letter_num(mltr[1]):letter_num(mltr[2])]
      ltr <- append(ltr, myRange, after = posn)
      ltr <- ltr[-posn]
      n <- n + length(myRange)
    }
  }
  
  letter_num(ltr)
}


#' as_numeric
#'
#'
#' @rdname as_irgenwas
#' @param x objekt
#' @param na.string missing
#' @param dec  decimal
#' @param exclude.symbols  plus und  minus sind erlaubt
#'
#' @param ... alles
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


#' @export
as_numeric.numeric <- function(x, ...)
  return(x)



#' @export
as_numeric.character <-
  function(x,
           na.string = "",
           dec = ".",
           exclude.symbols =  "[^0-9.\\+\\-]",
           ...) {
    lbl <- attr(x, "label")
    
    x <- sub("[[:space:]]+$", "", x)
    x <- sub("^[[:space:]]+", "", x)
    if (dec != ".")
      x <- gsub(dec, ".", x)
    
    
    x[which(x %in% na.string)] <- NA
    x[which(ifelse(is.na(x), NA,  grepl(exclude.symbols, x)))] <- NA
    x <- gsub(exclude.symbols, "", x)
    x <- as.numeric(x)
    attr(x, "label") <- lbl
    x
  }

#' @rdname as_irgenwas
#' @export
as_numeric.factor <-   function(x,
                                na.string = "",
                                dec = ".",
                                exclude.symbols =  "[^0-9.\\+\\-]",
                                ...) {
  lbl <- attr(x, "label")
  lvl <- as_numeric(levels(x), na.string,
                    dec,
                    exclude.symbols)
  if (!all(is.na(lvl))) {
    levels(x) <-   lvl
    x <- as.character(x)
  }
  
  x <- as.numeric(x)
  attr(x, "label") <- lbl
  x
}



# as_numeric2 <- function(x, neg.value = FALSE) {
#   lbl <- attr(x, "label")
#
#   if (neg.value)
#     x  <-  gsub("[^0-9,.-]", "", as.character(x))
#   else
#     x  <-  gsub("[^0-9,.]", "", as.character(x))
#
#   x <-  as.numeric(gsub(",", ".", x))
#   attr(x, "label") <- lbl
#   x
# }




#' as_logical
#'
#' @param x Objekt
#'
#' @return logical
#' @export
#'
as_logical <- function(x) {
  lbl <- attr(x, "label")
  
  if(is.numeric(x)) x == 1
  else if (nlevels(x) == 2)
    x <- x == levels(x)[1]
  else stop("Die Funktion  as_logical kann nur mit zwei Levels arbeiten.")
  attr(x, "label") <- lbl
  x
}
#' as_factor
#'
#' @param x Objekt
#' @param ... an factor
#'
#' @return factor
#' @export
#'
as_factor <- function(x, ...) {
  if (inherits(x, "haven_labelled"))
    haven::as_factor(x)
  else {
    lbl <- attr(x, "label")
    x <- factor(x, ...)
    
    attr(x, "label") <- lbl
    x
  }
  
}
 
#' as_cut
#'
#' @param x Objekt
#' @param ... an cut
#'
#' @return factor
#' @export
#'
as_cut <- function(x, ...) {
  lbl <- attr(x, "label")
  x <- cut(as.numeric(x), ...)
  
  attr(x, "label") <- lbl
  x
  
}


#' rev.factor
#'
#' @param x  Objekt
#'
#' @return factor
#' @export
 
rev.factor<- function(x){
  lbl <- attr(x, "label")
  x <- factor(x, rev(levels(x)))
  
  attr(x, "label") <- lbl
  x
  
}