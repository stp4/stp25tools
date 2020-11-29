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
#' copie of sjlabelled::as_numeric
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' dummy <- factor(c("3", "4", "6"))
#' as_numeric(dummy)
as_numeric <- function(...){ sjlabelled::as_numeric(...) }

