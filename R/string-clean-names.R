#' clean_names
#' 
#' Alternative zu `janitor`
#' Quelle https://drdoane.com/clean-consistent-column-names/
#'
#' @param data data.frame oder character
#' @param tolower  alles zu kleinbuschstaben
#' @param unique eindeitige namen
#' @param abbreviate,minlength  Abbkürzung
#'
#' @return die selbe Klasse wie der Input
#' @export
#' @examples
#'
#'
#' clean_names(tibble::tibble("Öli"=1:3, "p-k"=1:3, "95%-CI"=4:6) )
#'
#' c(
#'   c(
#'     "  a", "a  ", "a %", "a",
#'     "$a", "$$$a", "GDP ($)",
#'     "GDP (us$)", "a (#)", "a & b",
#'     "#", "$", "a_cnt", "Aa&Bb",
#'     "camelCasePhrases", "AlphaBetaGamma",
#'     "Alpha       Beta", "Beta  !!! Gamma",
#'     "a + b", "a - b", "a * b", "Ösel"
#'   ), abbreviate=TRUE
#' )
#'
#' 
#' 
clean_names <- function(x, ...) {
  UseMethod("clean_names")
}

#' Fix names of data.frames
#' 
#' Insbesondere bei `readxl::read_excel` werden die Spaltennamen
#' genau so übernommen, wie sie in Excel vorkommen, also mit allen Sonderzeichen.
#' Diese Funktion packt die Namen in die Labels und bereinigt die Spaltennamen.
#' Die Funktion ist praktisch die gleiche wie `clean_names()`, aber `fix_names()` 
#' ist einfacher aufgebaut. 
#' `clean_names()` enthält die Funktion `cleanup.encoding()`.
#' #'
#' @param x Data.frame
#' @param tolower  lower case
#' @param abbreviate,name_length Abbreviate strings to at least name_length characters,
#' @param replace character to replace
#'
#' @return Data.frame
#' @export
#'
#' @examples
#' 
#' #   fl_new <- "Raw data/Mai_Markiert_LT_Cases_Jan2018-Dez2022.xlsx"
#' # DF <- readxl::read_excel(fl_new, range = "A1:z15", sheet = 1) |> 
#' #   fix_names()
fix_names <- function(x,
                      tolower = TRUE,
                      abbreviate = TRUE,
                      name_length = 25,
                      replace =
                        c(
                          "%" =  "_pct",
                          "#" =  "_cnt",
                          "\u00A8" = "",
                          "&+" = "_and_",
                          "@+" = "_at_",
                          "_" = "."
                        )) {
  labl <- names(x)
  
  mns <- clean_names2(
    labl,
    tolower = tolower,
    abbreviate = abbreviate,
    minlength = name_length,
    replace = replace
  )
  
  names(labl) <- mns
  names(x) <- mns
  set_label(x, labl)
}

#' @rdname clean_names
#' @description clean_names2() ist eine einfachere Variante von clean_names(). 
#' Die Funktion gibt es nur für Vektoren. Die Umlaute werden nicht mit Ue oder oe
#' übersätzt.
#' 
#' 
#' @export
clean_names2 <- function(x,
                         tolower = TRUE,
                         abbreviate = TRUE,
                         minlength = 25,
                         replace =
                           c(
                             "%" =  "_pct",
                             "#" =  "_cnt",
                             "\u00A8" = "",
                             "&+" = "_and_",
                             "@+" = "_at_",
                             "_" = "."
                           )) {
  x <- stringi::stri_trans_general(x, "latin-ascii")
  if (tolower) x <- tolower(x)
  if(!is.null(replace)) x <- stringr::str_replace_all(str = x, pattern = replace)
  x <- gsub("[^a-zA-Z0-9_]+", "\\.", x)
  if (abbreviate) {
    x <- abbreviate(x, minlength = 25, named = FALSE)
    x <- gsub("\\.+", ".", x)
  }
  x <- gsub("(^\\.+|\\.+$)", "", x)
  
  make.names(x, unique = TRUE)
}


#' @rdname clean_names
#' 
#' @param label,labels label
#' @param cleanup.encoding,from,to logical  UTF-8 to latin
#' @param ... alles 
#' @description  clean_names(): Input data.frame  output ist ein 
#' data.frame mit bereinigten namen.
#' @export
#' @examples 
#' #' 
#' df <- data.frame(
#'   Öse = c(1, 2, 3, 1, 2, 3),
#'   Löre = gl(2, 3, labels = c("Amy", "Bob")),
#'   Fürn = c(9, 7, 6, 8, 6, 9),
#'   Mäße = c(6, 7, 8, 5, 6, 7),
#'   hüne=c(1, 2, 3, 1, 2, 3)
#' )
#' 
#' # Beide funktionen machen das gleiche 
#' # nur fix_names ist etwas einfacher gestrickt.
#' get_label(clean_names(df))
#' 
#' get_label(fix_names(df))
#' 
clean_names.data.frame <-
  function(x,
           label = TRUE,
           labels = NULL,
           cleanup.encoding = FALSE,
           from = "UTF8" , 
           to = "latin1",
           ...) {
    nams_df <- names(x)
    if (cleanup.encoding)
      nams_df <- iconv(nams_df, from, to)
    
    nams_clean <- clean_names.default(nams_df, ...)
    
    
    if (label) {
      if (is.null(labels)) {
        labels <- nams_df
      }
      else if (length(labels) != length(nams_clean)) {
        stop(" Laenge der labels muss gleich der laenge des DF sein!")
      }
      names(x) <- nams_clean
      
      if (cleanup.encoding)
        labels <- iconv(labels, from, to)
      names(labels) <- nams_clean
      label_data_frame(x, labels)
    }
    else {
      names(x) <- nams_clean
      x
    }
  }


#' @rdname clean_names
#' @param x  objekt
#'
#' @param replace named character string
#' 
#' @export
clean_names.default <-
  function(x,
           tolower = TRUE,
           unique = TRUE,
           abbreviate = FALSE, 
           minlength = 4,
           replace =
             c(
               "'" = "",
               "\"" =  "",
               "%" =  "_pct",
               "#" =  "_cnt",
               "\u00e4" = "ae",
               "\u00fc" = "ue",
               "\u00f6" = "oe",
               "\u00dc" = "Ue",
               "\u00c4" = "Ae",
               "\u00d6" = "Oe",
               "\u00df" = "ss",
               #   diaeresis <- "\u00A8" Sonderzeichen aus socisurvy
               "\u00A8" = "",
               # "\\++" = "_plus_",
               # "-+" = "_minus_",
               # "\\*+" = "_star_",
               
               "&+" = "_and_",
               "@+" = "_at_",
               "_" = "."
             ),
           ...) {
    n <- stringr::str_replace_all(str = x,
                                  pattern = replace)
    n <- trimws(n)
    n <- gsub("[^a-zA-Z0-9_]+", "\\.", n)
    # n <- gsub("([A-Z][a-z])", "_\\1", n)
    n <- gsub("(^\\.+|\\.+$)", "", n)
    n <- make.names(n)
    
    if (tolower)
      n <- tolower(n)
    
    if (abbreviate) {
      n <- gsub("\\.+", "", n)
      n <- abbreviate(n, minlength, named = FALSE)
    }
    
    if (unique)
      n <- make.unique(n, sep = ".")
    
    n
  }


#' @rdname clean_names
#' @description 
#' paste_names:  paste names
#'
#' @param collapse  an  character string to separate the results ", "
#'
#' @return character
#' @export
#'
paste_names <-
  function(x, 
           collapse = ", ",
           ...) {
    paste0(names(x) , collapse = collapse)
  }


#' @rdname clean_names
#' @description 
#' paste_names_levels:  paste names + Labels Die funktion ist gedacht für 
#' Copy and Paste.
#'
#' @param collapse  an  character string to separate the results ", "
#'
#' @return character
#' @export
#'
paste_names_levels <- function(x, 
                               abbreviate = TRUE, 
                               collapse = ", ",
                               ...) {
  strg <-  get_label(x)
  if (abbreviate)
    paste0(names(strg), " = '", abbreviate(as.vector(strg), 15), "'", collapse = collapse)
  else
    paste0(names(strg), " = '", abbreviate(as.vector(strg), 15), "'", collapse = collapse)
}


#' @rdname clean_names
#' @description 
#' cleansing_umlaute(): 
#' Funktion entfernt stoerende Umlaute, unten stehende Liste ggf. erweitern.
#'
#' @param x string
#' @export
#'
cleansing_umlaute <- function(x,
                              ...){
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



#' @rdname clean_names
#' @description  cleansing_umlaute2: Sonderzeichen aus socisurvy
#' @export
cleansing_umlaute2 <- function(x, ...) {
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

# Wird in stp25projekt verwendet
#
# #-- Functions stolen from library(report)
# scrubber <- function(text.var, 
#                      rm.quote = TRUE, 
#                      fix.comma = TRUE, ...){
#   x <- reducer(Trim(clean(text.var)))
#   if (rm.quote) {
#     x  <- gsub('\"', "", x)
#   }
#   if (fix.comma) {
#     x <- gsub(" ,", ",", x)
#   }
#   ncx <- nchar(x)
#   x <- paste0(Trim(substring(x, 1, ncx - 1)), substring(x, ncx))
#   x[is.na(text.var)] <- NA
#   x
# }
# # sprintf("%X", as.integer(charToRaw("Ae")))
# 
# 
# #internal not exported
# reducer <- function(x) gsub("\\s+", " ", x)
# 
# #internal not exported
# Trim <- function (x) gsub("^\\s+|\\s+$", "", x)
# 
# #internal not exported
# unblanker <- function(x) subset(x, nchar(x)>0)
# 
# #internal not exported
# clean <- function(text.var) sub("\\s+", " ", gsub("\r|\n|\t", " ", text.var))
# 
# mgsub <- function(pattern, 
#                   replacement = NULL, 
#                   text.var,
#                   fixed = TRUE, ...){
#   key <- data.frame(pat=pattern, rep=replacement,
#                     stringsAsFactors = FALSE)
#   msubs <-function(K, x, ...){
#     sapply(seq_len(nrow(K)), function(i){
#       x <<- gsub(K[i, 1], K[i, 2], x, fixed = fixed, ...)
#     }
#     )
#     return(gsub(" +", " ", x))
#   }
#   x <- Trim(msubs(K=key, x=text.var, ...))
#   return(x)
# }


 
#' @rdname clean_names
#' @param x  string
#' 
#' @export
#' @examples 
#' abbreviate2(c("Hallö", "Halü"))
#' 
abbreviate2 <- function(x, 
                        minlength = 4)
  abbreviate(
    stringi::stri_trans_general(x, "latin-ascii"),
    minlength = 4, 
    named = FALSE
    )

 



