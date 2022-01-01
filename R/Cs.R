#' Character strings from unquoted names
#'
#' Erweiterung der  Hmisc Cs-Funktion um trim.ws. Erlaubt Variablen mit Komma oder Plus(+) oder Leerzeichen abzuternnen.
#' @param ... Vektor oder String oder Formula
#' @return Vektor
#' @export
#' @examples
#' 
#' Cs(sd, fr, fg)
#' Cs(sd, fr, fg, "hju nh")
#' Cs("  Hallo Welt ")
#' Cs(~ sd + fr + fg)

Cs<-
  function (...)
  {
    x <- as.character(sys.call())[-1]
    if (length(x) == 1)
      strsplit(gsub("^[[:blank:]]*", "",
                    gsub(
                      "[[:blank:]]*$", "",
                      gsub("[\t\n\\~\\+\\:/]", "  ", x)
                    ))
               , " +")[[1]]
    else
      x
    
  }
 




