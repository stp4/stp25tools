# wrap_string



#' wrap_string (umbrechen)
#' 
#' Kopie von  str_wrap  wobei die Labels mitupData2 ergaenzt werden wenn ein 
#' Data.Frame-Objekt uebergeben wird. 
#' 
#' wrap_label kuerzt die label fur Grafiken.
#' 
#'
#' @param x data.frame oder String
#' @param width default width= 25
#' @param sep  default new line
#' @param pattern,replacement  zB Unterstriche _ ersaetzen
#' @param max.lines,max.lines.char  Anschneiden des Strings
#' 
#' @return Character String
#' @export
#' 
#' @examples
#' 
#' wrap_string(
#'   "R is free   software and comes with ABSOLUTELY NO WARRANTY.
#'   You are welcome to redistribute it under certain conditions.
#'   ", 5
#' )
#' 
#' 
#' 
#' 
#' df <- data.frame(
#'   BMI=c(1,2,3,1,2,3),
#'   WHtR= gl(2,3, label =c("R is free   software  and comes with ABSOLUTELY NO WARRANTY", 
#'                          "You are welcome to redistribute it under certain conditions.")),
#'   WHtR_1=c(9,7,6,8,6,9),
#'   bildprof=c(6,7,8,5,6,7)
#' )
#' 
#' DF<-
#'   Label(df, BMI = "Body-Mass-Index   Masszahl für die Bewertung des Koerpergewichts eines
#'    Menschen in Relation zu seiner Koerpergroesse.",
#'         WHtR =  "Waist-Height-Ratio",
#'         WHtR_1 ="Waist-Height-Ratio"
#'   )
#' 
#'   # DF$BMI<- units::set_units(DF$BMI, kg/m2)
#' 
#' DF<- wrap_label(DF,   width = 20, max.lines = 1)
#' get_label(DF)
#' 
#' wrap_factor(DF, max.lines = 1, max.lines.char = "")
#' 
#' 
wrap_string <- function(x, ...) {
  UseMethod("wrap_string")
}

#' @export
wrap_string.character <- function(x,
                          width = 25,
                          sep =  "\n",
                          pattern=NULL, 
                          replacement=NULL,
                          max.lines = NULL,
                          max.lines.char=" ..."){
    if (!is.null(pattern))
      x <- gsub(pattern, replacement, x)
    
  x <- gsub("\\s+", " ", x, perl=TRUE)
  x <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", x, perl=TRUE)
 
  
  # NODE                     EXPLANATION
  # --------------------------------------------------------------------------------
  #   (?<=                     look behind to see if there is:
  #      ---------------------------------------------------------------------------
  #      [\s]                  any character of: whitespace (\n, \r, \t, \f, and " ")
  #    -----------------------------------------------------------------------------
  #   )                        end of look-behind
  # --------------------------------------------------------------------------------
  #   \s*                      whitespace (\n, \r, \t, \f, and " ") (0 or
  #                            more times (matching the most amount possible))
  # --------------------------------------------------------------------------------
  #   |                        OR
  # --------------------------------------------------------------------------------
  #   ^                        the beginning of the string
  # --------------------------------------------------------------------------------
  #   \s+                      whitespace (\n, \r, \t, \f, and " ") (1 or more times 
  #                            (matching the most amount possible))
  # --------------------------------------------------------------------------------
  #   $                        before an optional \n, and the end of the string
  
   if (!is.null(sep)){
  
  x <-
    stringi::stri_wrap(
      x,
      width = width,
      indent = 0,
      exdent = 0,
      simplify = FALSE
    )

  x <-  vapply(x,  stringr::str_c, collapse = sep, character(1))
  } 
  if (!is.null(max.lines)) {
    x_split <- strsplit(x, sep)
    if (!all(lengths(x_split) == 1))
      x <-
        sapply(x_split,
               function(y) {
                 r <-  y[1:max.lines]
                 r[is.na(r)] <- ""
                 r <- paste(r, collapse = sep)
                 if (length(y) > max.lines) {
                   r <- paste0(r, max.lines.char)
                 }
                 r
               })
  }
  
  x
}

 

 
#' @export
wrap_string.data.frame  <-
  function(x,
           width = 20,
           sep = "\n",
           pattern = "_",
           replacement = " ",
           max.lines = NULL,
           max.lines.char = "...") {
    if (is.data.frame(x)) {
      lvl <- wrap_string(get_label(x),
                         width,
                         sep,
                         pattern,
                         replacement,
                         max.lines,
                         max.lines.char)
      names(lvl) <- names(x)
      set_label(x, lvl)
    }
    else{
      wrap_string(x,
                  width,
                  sep,
                  pattern,
                  replacement,
                  max.lines,
                  max.lines.char)
    }
  }



#' @export
wrap_string.factor  <-
  function(x,
           width = 20,
           sep = "\n",
           pattern = "_",
           replacement = " ",
           max.lines = NULL,
           max.lines.char = "...") {
    if (is.factor(x)) {
      factor(
        x,
        lvl,
        wrap_string(
          lvl,
          width,
          sep,
          pattern,
          replacement,
          max.lines,
          max.lines.char
        )
      )
      
    }
    if (is.data.frame(x)) {
      for (i in      which(sapply(x, is.factor))) {
        lvl <- levels(x[[i]])
        x[[i]] <-  factor(
          x[[i]],
          lvl,
          wrap_string(
            lvl,
            width,
            sep,
            pattern,
            replacement,
            max.lines,
            max.lines.char
          )
        )
        
      }
      x
    }
    else{
      stop(" Nicht möglich!")
      #wrap_string(x, width, sep, pattern, replacement, max.lines)
    }
  }

# 
# .wrap_string <- function(x, width, sep, pattern, replacement,max.lines) {
#  
#   if (!is.null(pattern))
#     x <- gsub(pattern, replacement, x)
#  
#   
#   wrap_string( x,
#                width = width,
#                sep =  sep,
#                max.lines=max.lines)
# }
# 


