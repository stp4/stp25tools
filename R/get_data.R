#' get_data
#' 
#' 
#' Daten importieren:
#' 
#'  xlsx: readxl::read_excel(file, sheet, skip, range)
#' 
#'  csv:   read.table(file, header, sep, quote, dec, na.strings, skip, fill, comment.char)
#'  
#'  sav:  haven::read_sav(file, encoding,  user_na)
#'  
#'  Text: read.text2(file, dec)
#'  
#'
#' @param file Demo.xla,
#' Demo.sav,
#' Demo.csv  - ist ein Pfad zu einem csv, xls, sav oder Rdata Datensatz
#' oder ein String in Textformat dierekt im R-File.
#' @param na.strings c(NA,9999,"") - Fehlende Werte
#' @param tabel_expand logical FALSE - Tabellen mit haufigkeiten werden als Dataframe im long-Format ausgegeben
#' @param id.vars nur mit tabel_expand  - Nummer und Name der ID-Variablen bei tabel_expand default ist 1.
#' @param value nur mit tabel_expand  - Name der output-variable bei tabel_expand.
#' @param quote Lesen der csv- Files = ";", = "\"", ".",
#' @param sheet,skip,range an readxl::read_excel
#' @param cleanup.encoding,cleanup.names,cleanup.factor   cleanup UTF-8 = FALSE,
#' @param user_na	If TRUE variables with user defined missing will be read into labelled_spss objects. If FALSE, the default, user-defined missings will be converted to NA.
#' @param label use attribut label
#' @param as_tibble output
#' @param encoding,from,to Encoding
#' @param header an SPSS
#' @param fill an CSV
#' @param comment.char an csv
#' @param ...  Argumente fuer spss und csv. Bei SPSS-Files  kann die Zeichencodierung mit  \code{cleanup.encoding ="UTF-8"} geaendert werden.
#'
#' @export
#' @examples
#' # require(stp25tools)
#' 
#' dat <- get_data("
#' sex treatment control
#' m  2 3
#' f  3 4
#' ",
#'                 tabel_expand = TRUE,
#'                 id.vars = 1)
#' 
#' xtabs(~ sex + value, dat)
#' 
#' 
#' dat <- get_data(
#'   "
#' sex treatment  neg  pos
#' f   KG          3   3
#' f   UG          4   5
#' m   KG          5   4
#' m   UG          4   2
#' ",
#'   tabel_expand = TRUE,
#'   id.vars = 1:2,
#'   value = "befund"
#' )
#' 
#' ftable(xtabs(~ sex + treatment + befund, dat))
#'  
#'  \dontrun{
#' if(file.exists("R/dummy.csv")){
#'   
#'   get_data("R/dummy.csv", dec = ",", na.strings = "-", skip=1, label=1)
#'   get_data("R/dummy.xlsx", na.strings = "-")
#'   get_data("R/dummy.xlsx")
#'   
#'   x <- get_data("R/dummy.sav")
#'   get_label(x)[1:4]
#'   
#' }
#' }
get_data <- function (file = NA,
                      na.strings = NULL,
                      dec = ".",
                      sep = ";",
                      sheet = 1,
                      range = NULL,
                      skip = 0,
                      label = 0,
                      as_tibble = TRUE,
                      cleanup.names = TRUE,
                      cleanup.encoding = FALSE,
                      cleanup.factor = TRUE,
                      tabel_expand = FALSE,
                      id.vars = 1,
                      value = "value",
                      user_na = FALSE,
                      from = "UTF8" ,
                      to = "latin1",
                      encoding = NULL,
                      # sav
                      header = TRUE,
                      # csv
                      fill = TRUE,
                      comment.char = "",
                      quote = "\"",
                      ...){
  
  read_xlsx <- function() {
    if (is.null(na.strings)) {
      dat <-
        readxl::read_excel(file,
                           sheet = sheet,
                           skip = skip,
                           range = range)
    }
    else{
      dat <-
        readxl::read_excel(
          file,
          sheet = sheet,
          skip = skip,
          range = range,
          na = na.strings
        )
    }
    if (cleanup.names) {
      fix_names(dat)
    }
    else {
      dat
    }
  }
  
  read_csv <- function() {
    dat <-
    read.table(
      file = file,
      header = header,
      sep = sep,
      quote = quote,
      dec = dec,
      na.strings = na.strings,
      skip = skip,
      check.names = FALSE,
      fill = fill,
      comment.char = comment.char
    )
    
    if(cleanup.names) { 
      fix_names(dat)
    }
    else {dat}
    
      # if( skip > 0 & label > 0){
      # label <-
      #   read.table(
      #     file = file,
      #     header = FALSE,
      #     sep = sep,
      #     quote = quote,
      #     skip = label - 1,
      #     nrows = 1
      #   )
      #   
      #   if (any(is.na(label))) {
      #     isna <- which(is.na(label))
      #     
      #     label[isna] <- names(dat)[isna]
      #   }
      #   names(label) <-  names(dat)
      #   dat <- set_label2(dat, label)
      # } 
     # dat
  }
  
  read_sav <- function() {
    dat <-
      haven::read_sav(file, encoding = encoding, user_na = user_na)
    dat <-  haven::as_factor(dat)
    
    if (cleanup.names) {
      clean_names(dat, label = FALSE)
    } else  {
      dat
    }
  }
  
  data <- data.frame(NULL)
  if (length(grep("\n", file)) > 0) {
    # cat("\n\nread-text\n")
    # workaround
    # ich habe nachträglich die tabs eingebaut!
    if( sep == ";" ) sep <- "" 
    data <-
      read.text2(file,
                 na.strings = na.strings,
                 sep = sep,
                 dec = dec)
  }
  else {
    if (file.exists(file)) {
      # cat("\n\nread-file\n")
      file_info <- file.info(file)[c(1, 4, 5)]
      ext <- tolower(tools::file_ext(file))
      
      data <-
        switch(ext,
               sav = read_sav(),
               xlsx = read_xlsx(),
               csv =  read_csv(),
               stop("Unknown extension '.", ext, "'", call. = FALSE))
      
      if (cleanup.encoding) {
        data <- names_label_encoding(data)
        data <- character_encoding(data)
        data <- factor_levels_encoding(data)
      }
    }
    else {
      stop("Kein file: ", file, " vorhanden!")
    }
  }
  
  
  if (tabel_expand) {
    data <-
      expand_table(data, id.vars = id.vars, value = value, ...)
  }
  
  if (as_tibble & !tibble::is_tibble(data)) 
    data <- tibble::tibble(data)
  else if (!as_tibble & tibble::is_tibble(data)) 
    data <- data.frame(data)
  else NULL
  
  data
}


#' cleanup_NA
#'
#'
#'
#' @param obj data.frame
#' @param na.strings na als character
#' @param force.numeric alles zu Nummern
#'
#' @return data.frame
#' @noRd
cleanup_NA <-
  function(obj,
           na.strings = NULL,
           force.numeric = FALSE) {
    dimobj <- dim(obj)
    for (i in 1:dimobj[2]) {
      x <- obj[[i]]
      if (!is.null(na.strings)) {
        x[x %in% na.strings] <- NA
        modif <- TRUE
      }
      if (force.numeric && length(lev <- levels(x))) {
        x <- factor(x)
        if (Hmisc::all.is.numeric(levels(x))) {
          x <- as.numeric(as.character(x))
          modif <- TRUE
        }
      }
      if (modif)
        obj[[i]] <- x
      NULL
    }
    obj
  }

#' @rdname get_data
#' @description  Read Text Lines
#'
#' @param string  character string
#' @param na.strings 	  a character vector of strings which are to be interpreted as NA values.
#' @param sep 	  the field separator character.
#' @param dec  the character used in the file for decimal points.
#' @param stringsAsFactors TRUE
#' @export
#'
#' @return data.frame
read.text2 <-
  function (string,
            na.strings = c("NA", "na"),
            sep = "", # the separator is ‘white space’
            dec = ".",
            stringsAsFactors = TRUE) {
    
    if(sep == " ") string <- gsub("\t", " ", string)

    data <- read.table(
      zz <- textConnection(string),
      header = TRUE,
      sep = sep,
      dec = dec,
      na.strings = na.strings,
      stringsAsFactors = stringsAsFactors
    )
    close(zz)
    data
  }


#' Tabel To Expand Data Frame
#'
#' @param data data.frame
#' @param id.vars idetifier
#' @param value name of value
#' @param as.is,dec,na.strings  an  type.convert(...)
#'
#' @return data.frame
#'
#' @examples
#' 
#' dat <- expand_table(
#'   data.frame(
#'     sex = c("f", "f", "m", "m"),
#'     treatment = c("KG", "UG", "KG" , "UG"),
#'     neg  = c(3, 4, 5, 4),
#'     pos = c(3, 5, 4, 2)
#'   )
#'   ,
#'   id.vars = 1:2,
#'   value = "befund"
#' )
#' 
#' xtabs(~ befund + sex + treatment, dat)
#' 
#' @noRd 
expand_table <-
  function(data,
           id.vars,
           value = "value",
           na.strings = "NA",
           as.is = FALSE,
           sep = " ",
           dec = ".") {
    if (is.character(data))
      data <-
        read.text2(data,
                   na.strings = na.strings,
                   sep = sep,
                   dec = dec)  # nur wenn die Funktion dierekt aufgerufen wird moeglich
    
    
    if (!is.numeric(id.vars))
      id.vars <- which(names(data) %in% id.vars)
    
    dataMatrix <- as.matrix(data[, -id.vars])
    
    if (length(id.vars) == 1) {
      dimnames(dataMatrix)[[1]] <- data[, 1]
      data2 <-
        expand_dft(as.data.frame(as.table(dataMatrix),
                                 stringsAsFactors = TRUE),
                   na.strings,
                   as.is,
                   dec)
      colnames(data2)[2] <- value
    }
    else {
      dimnames(dataMatrix)[[1]] <- apply(data[, id.vars], 1, paste,
                                         collapse = "+")
      data2 <-
        expand_dft(as.data.frame(as.table(dataMatrix),
                                 stringsAsFactors = TRUE),
                   na.strings,
                   as.is,
                   dec)
      colnames(data2)[2] <- value
      
      data2 <-
        cbind(reshape2_colsplit(data2[, 1], "\\+", names(data)[id.vars]),  data2)
    }
    
    data2 <-
      as.data.frame(lapply(data2, function(x)
        if (is.character(x))
          factor(x)
        else
          x))
    if (length(id.vars) == 1)  {
      names(data2)[1] <- names(data)[id.vars]
    }
    
    data2
  }


#' Helper for expand_table
#'  
#' http://wiki.stdout.org/rcookbook/Manipulating%20data/Converting%20between%20data%20frames%20and%20contingency%20tables/
#' @noRd
expand_dft <-
  function(x,
           na.strings = "NA",
           as.is = FALSE,
           dec = ".") {
   # Take each row in the source data frame table 
   #  and replicate it using the Freq value
    data <- sapply(1:nrow(x),
                   function(i)
                     x[rep(i, each = x$Freq[i]),],
                   simplify = FALSE)
    
    # Take the above list and rbind it to create a single data
    # Also subset the result to eliminate the Freq column
    data <-
      subset(do.call("rbind", data), select = -Freq)
    
    # Now apply type.convert to the character coerced factor columns
    # to facilitate data type selection for each column
    for (i in 1:ncol(data)) {
      data[[i]] <-
        type.convert(
          as.character(data[[i]]),
          na.strings = na.strings,
          as.is = as.is,
          dec = dec
        )
    }
    data
  }


# cleanup_haven_factor <- function(data,
#                                  ...) {
#   y <- sapply(data, function(x) {
#     if (inherits(x, "haven_labelled"))
#       TRUE
#     else if (inherits(x, "labelled"))
#       TRUE
#     else
#       FALSE
#   })
#   
#   if (sum(y) > 0)
#     data[which(y)] <-
#       lapply(data[which(y)], haven::as_factor)
#   
#   if (tibble::is_tibble(data))
#     data
#   else
#     tibble::as_tibble(data)
# }



#' @noRd
character_encoding <- function(data,
                               from = "UTF8",
                               to = "latin1") {
  myFact <-
    which(sapply(data, function(x)
      inherits(x, "character")) == TRUE)
  if (length(myFact) > 0) {
    for (i in myFact)
      data[, i] <-  iconv(data[, i], from , to)
  }
  data
}



#' @noRd
factor_levels_encoding <- function(data,
                                   from = "UTF8",
                                   to = "latin1") {
  myFact <-
    which(sapply(data, function(x)
      inherits(x, "factor")) == TRUE)
  if (length(myFact) > 0) {
    for (i in myFact)
      levels(data[, i]) <-  iconv(levels(data[, i]), from , to)
  }
  data
}



#' @noRd
names_label_encoding <- function(data,
                                 from = "UTF8",
                                 to = "latin1") {
  nms <-   iconv(names(data), from , to)
  lbl <- get_label(data)
  lbl <-   iconv(lbl, from , to)
  names(lbl) <- nms
  names(data) <- nms
  lbl <- set_label(data, lbl)
  data
}




#' Split a vector into multiple columns
#' 
#' Stolen from reshape2
#' 
#' reshape2 ist ein altes packages und womoewglich bald osolet!
#'
#' @param string character vector or factor to split up
#' @param pattern regular expression to split on
#' @param names names for output columns
#'
#' @examples
#' 
#' #'\dontrun{
#' 
#' x <- c("a_1", "a_2", "b_2", "c_3")
#' vars <- stp25tools:::reshape2_colsplit(x, "_", c("trt", "time"))
#' vars
#' str(vars)
#' }
#' 
#' @noRd
reshape2_colsplit<-
  function (string, pattern, names) 
  {
    vars <- stringr::str_split_fixed(string, pattern, n = length(names))
    df <- data.frame(plyr::alply(vars, 2, type.convert, as.is = TRUE), 
                     stringsAsFactors = FALSE)
    names(df) <- names
    df
  }



# Convert_To_Factor
# 
# interne funktion fuer factor und cleanup
# labels lassen sich nicht direkt auslesen
# 
# SPSS erlaubt leere Labels daher diese auffuellen
# 
# x <- haven::as_factor(x) Labels werden Falsch geordnet wenn zB
# 
#  1=ja, 8=nein dabei wird 8 verworfen
 
# Convert_To_Factor <- function(x) {
#   lbl <-
#     if (any(names(attributes(x)) == "label"))
#       attr(x, "label")
#   else
#     names(x)
#   
#   lbls <- attr(x, "labels")
#   
#   if (any(names(lbls) == "") | any(names(lbls) == "&nbsp;")) {
#     spss_names <- gsub("&nbsp;", "", names(lbls))
#     empty <- which(spss_names == "")
#     names(lbls)[empty] <- empty
#   }
#   x <- factor(x, lbls, names(lbls))
#   attr(x, "label") <- lbl
#   x
# }

# Hmisc::all.is.numeric
# function (x, what = c("test", "vector", "nonnum"), extras = c(".", 
#                                                               "NA")) 
# {
#   what <- match.arg(what)
#   x <- sub("[[:space:]]+$", "", x)
#   x <- sub("^[[:space:]]+", "", x)
#   xs <- x[x %nin% c("", extras)]
#   if (!length(xs) || all(is.na(x))) 
#     return(switch(what, test = FALSE, vector = x, nonnum = x[0]))
#   isnon <- suppressWarnings(!is.na(xs) & is.na(as.numeric(xs)))
#   isnum <- !any(isnon)
#   switch(what, test = isnum, vector = if (isnum) suppressWarnings(as.numeric(x)) else x, 
#          nonnum = xs[isnon])
# }
