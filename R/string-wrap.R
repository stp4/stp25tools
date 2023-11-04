#' wrap_string (umbrechen)
#'
#'
#'  wrap_string(), wrap_factor(), wrap_data_label(),   wrap_string_at(), split_string()
#'
#' Kopie von  str_wrap  wobei die Labels mitupData2 ergaenzt werden wenn ein
#' Data.Frame-Objekt uebergeben wird.
#'
#'
#'
#' @param x data.frame oder String
#' @param width default width= 25
#' @param sep  default new line
#' @param pattern,replacement  zB Unterstriche _ ersaetzen
#' @param max.lines,max.lines.char  Anschneiden des Strings
#' @name wrap_string
#' @param ... alles weiter
#'
#' @return wrap_string: string
#' @export
#'
#' @examples
#' 
#' strg<- c("R is free   software and comes with ABSOLUTELY NO WARRANTY.",
#'          "You are welcome to redistribute it under certain conditions.")
#' 
#' wrap_string(strg, 5)
#' wrap_string(factor(strg))
#' wrap_factor(factor(strg))
#' 
#' #wrap_data_label(data)
#' wrap_string_at(strg, "and")
#' split_string(strg, "and")
#'
wrap_string <- function(x, ...) {
  UseMethod("wrap_string")
}


#' @rdname wrap_string
#' @export
#' @return character
wrap_string.character <- function(x,
                                  width = 25,
                                  sep =  "\n",
                                  pattern = NULL,
                                  replacement = NULL,
                                  max.lines = NULL,
                                  max.lines.char = " ...",
                                  ...) {
  if (!is.null(pattern))
    x <- gsub(pattern, replacement, x)
  
  x <- gsub("\\s+", " ", x, perl = TRUE)
  x <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", x, perl = TRUE)
  
  
  if (!is.null(sep)) {
    x <-
      stringi::stri_wrap(
        x,
        width = width,
        indent = 0,
        exdent = 0,
        simplify = FALSE
      )
    
    x <-  vapply(x,
                 stringr::str_c,
                 collapse = sep, character(1))
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


#' @rdname wrap_string
#' @param lvl character levels
#' @export
#' @return factor
wrap_string.factor  <- function(x,
                                width = 20,
                                sep = "\n",
                                pattern = "_",
                                replacement = " ",
                                max.lines = NULL,
                                max.lines.char = "...",
                                lvl = levels(x),
                                ...) {
  wrap_string(lvl,
              width,
              sep,
              pattern,
              replacement,
              max.lines,
              max.lines.char)
  
  
}

#' @rdname wrap_string
#' @description wrap_sentence: Kopie von  str_wrap ruckgabe der Labels
#' @export
#' @return character (Labels)
wrap_string.data.frame <- function(x,
                                   ...) {
  if (is.data.frame(x))
    x <- get_label(x, include.units = TRUE)
  wrap_string(x, ...)
}

#' @rdname wrap_string
#' @export
#' @return wrap_factor(): factor
wrap_factor  <- function(x,
                         width = 20,
                         sep = "\n",
                         pattern = "_",
                         replacement = " ",
                         max.lines = NULL,
                         max.lines.char = "...",
                         lvl = levels(x),
                         ...) {
  factor(
    as.character(x),
    levels = lvl,
    labels =  wrap_string(
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


#' @rdname wrap_string
#' @export
#' @return  wrap_data: data.frame
wrap_data_label  <-
  function(x,
           width = 20,
           sep = "\n",
           pattern = "_",
           replacement = " ",
           max.lines = NULL,
           max.lines.char = "...",
           ...) {
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


#' @rdname wrap_string
#' @export
wrap_string_at <- function(x, ...) {
  UseMethod("wrap_string_at")
}


#' @rdname wrap_string
#' @export
#' @return character
wrap_string_at.character <-
  function(x,
           pattern,
           replacement = paste0("\n", pattern),
           ...) {
    gsub(pattern = pattern, replacement = replacement, x)
  }


#' @rdname wrap_string
#' @export
#' @return factor
wrap_string_at.factor <-
  function(x,
           pattern,
           replacement = paste0("\n", pattern),
           lvl = levels(x),
           ...) {
    factor(as.character(x),
           levels = lvl,
           labels =  wrap_string_at(lvl,
                                    pattern,
                                    replacement))
  }


#' @rdname wrap_string
#' @export
split_string <- function(x,
                         ...) {
  UseMethod("split_string")
}


#' @rdname wrap_string
#' @param pos  integer Position
#' @export
#' @return character
split_string.character <- function(x,
                                   pattern,
                                   pos = 1,
                                   ...) {
  stringr::str_trim(
    sapply(
      stringr::str_split(x, pattern = pattern), 
      "[", pos)
    )
}


#' @rdname wrap_string
#' @export
#' @return factor
split_string.factor <- function(x,
                                pattern,
                                pos = 1,
                                lvl = levels(x),
                                ...) {
  factor(as.character(x),
         levels = lvl,
         labels =  split_string(lvl,
                                pattern,
                                pos))
}