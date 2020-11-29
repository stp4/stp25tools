
# stp25Output -------------------------------------------------------------

#'  cleansing
#'
#' Data cleansing
#'
#' cleansing_umlaute(): Funktion entfernt stoerende Umlaute,
#' Funktion entfernt stoerende Umlaute, unten stehende Liste ggf. erweitern
#' sprintf("%X", as.integer(charToRaw("Ae")))
#' @param x string
#' @export
#'
cleansing_umlaute <- function(x){
  x <- gsub("\u00e4","ae", x)
  x <- gsub("\u00fc","ue", x)
  x <- gsub("\u00f6","oe", x)
  x <- gsub("\u00dc","Ue", x)
  x <- gsub("\u00c4","Ae", x)
  x <- gsub("\u00d6","Oe", x)
  x <- gsub("\u00df","ss", x)
  x <- gsub(" ", "_", x)
  x
}





#' @rdname cleansing_umlaute
#' @description Sonderzeichen aus socisurvy
#' @export
cleansing_umlaute2 <-
  function(x) {
    diaeresis <- "\u00A8"
    ae <-  paste0("a", diaeresis)
    ue <-  paste0("u", diaeresis)
    oe <-  paste0("o", diaeresis)
    Ue <-  paste0("U", diaeresis)
    Ae <-  paste0("A", diaeresis)
    Oe <-  paste0("O", diaeresis)
    x <- gsub(ae, "\u00e4", x)
    x <- gsub(oe, "\u00f6", x)
    x <- gsub(ue, "\u00fc", x)
    x <- gsub(Ae, "\u00c4", x)
    x <- gsub(Ue, "\u00dc", x)
    x <- gsub(Oe, "\u00d6", x)
    x
  }


#' clean_space
#'
#' Leerzeichen entfernen
#'
#' @param x string
#'
#' @noRd
clean_space <- function(x) {
  x <- sub("[[:space:]]+$", "", x)
  x <- sub("^[[:space:]]+", "", x)
  sub(",", ".", x)
}



