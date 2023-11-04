#  as_logical, rev.factor, as_numeric, as_factor, as_rev, cut_bmi







#' as_irgenwas
#' 
#' @name as_irgenwas
#'
#' @param x vector
#' @param ... weitere methoden
#' @return vector
#'
NULL



#' @rdname as_irgenwas
#' @description cut_bmi
#' 
#'  BMI: WHO   (kg/m2)
#'
#'      Very severely underweight 15
#'      Severely underweight 15-16
#'      Underweight 16-18.5
#'      Normal (healthy weight) 18.5-25
#'      Overweight 25-30
#'      Obese Class I (Moderately obese) 30-35
#'      Obese Class II (Severely obese) 35-40
#'      Obese Class III (Very severely obese) 40
#' 
#'
#' @param x vector
#' @param breaks,labels an cut 
#' @param n anzahl der BMI-Kategorien default = 4 Underweight,        Normal,    Overweight, Obese Class I 
#' 
#' @export
#' @examples 
#' 
#' table(cut_bmi(c(10, 15, 16, 18.5, 25, 30, 35, 40, 45)))
#' table(cut_bmi(c(10, 15, 16, 18.5, 25, 30, 35, 40, 45), n=4))
#' table(cut_bmi(c(10, 15, 16, 18.5, 25, 30, 35, 40, 45), n=5))
#' table(cut_bmi(c(10, 15, 16, 18.5, 25, 30, 35, 40, 45), n=6))
#' 
cut_bmi <- function(x,
                    breaks = c(-Inf, 15, 16, 18.5, 25, 30, 35, 40, Inf),
                    labels = c(
                      "Very severely underweight",
                      "Severely underweight",
                      "Underweight",
                      "Normal",
                      "Overweight",
                      "Obese Class I",
                      "Obese Class II",
                      "Obese Class III"
                    ),
                    n = 4) {
  if (n == 3)
    cut(x, breaks[c(1, 4:5,  9)], labels[c(3:5)])
  else if (n == 4)
    cut(x,  breaks[c(1, 4:6,  9)],  labels[c(3:6)])
  else if (n == 5)
    cut(x, breaks[c(1, 3:6,  9)], labels[c(2:6)])
  else if (n == 6)
    cut(x, breaks[c(1, 3:7,  9)], labels[c(2:7)])
  else if (n == 7)
    cut(x, breaks[c(1, 2:7,  9)], labels[c(1:7)])
  else
    cut(x, breaks, labels)
  
  
}






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
#' @param trim_ws  logical trim ws
#' @param ...  readr::parse_number trim_ws	Should leading and trailing whitespace (ASCII spaces and tabs) be trimmed from each field before parsing it?
#' @export
as_numeric.character <-
  function(x,
           na.string = "",
           dec = c(".", ","),
           trim_ws= TRUE,
          #exclude.symbols =  "[^0-9.\\+\\-]",
           ...) {
    lbl <- attr(x, "label")
    
    if(  length(dec) == 2 & "," %in% dec) { 
      x <- gsub(",", ".", x)
      x <- readr::parse_number(x, na=na.string, trim_ws=trim_ws)
      }
    else if( dec[1] == ","){
      x <- readr::parse_number(x, 
                               locale = readr::locale(decimal_mark = ","),
                               na=na.string, trim_ws=trim_ws)
    }
    else{
      x <- readr::parse_number(x, na=na.string, trim_ws=trim_ws)
    }
    
    # x <- sub("[[:space:]]+$", "", x)
    # x <- sub("^[[:space:]]+", "", x)
    # if (dec != ".")
    #   x <- gsub(dec, ".", x)
    # 
    # 
    # x[which(x %in% na.string)] <- NA
    # x[which(ifelse(is.na(x), NA,  grepl(exclude.symbols, x)))] <- NA
    # x <- gsub(exclude.symbols, "", x)
    # x <- as.numeric(x)
    attr(x, "label") <- lbl
    x
  }

#' @rdname as_irgenwas
#' @export
as_numeric.factor <-   function(x,
                                na.string = "",
                                dec = c(".", ","),
                                ...) {
  lbl <- attr(x, "label")
  # x <- as_numeric(as.character(x), na.string, dec)
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


#' @rdname as_irgenwas
#' @description as_factor: haven_labelled zu factor
#' @export
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
 

#' @rdname as_irgenwas
#' @description as_cut: cut mit label
#' @export
as_cut <- function(x, ...) {
  lbl <- attr(x, "label")
  x <- cut(as.numeric(x), ...)
  
  attr(x, "label") <- lbl
  x
  
}

 
#' @rdname as_irgenwas
#' @description as_rev: reverse factor
#' @export
rev.factor <- as_rev <- function(x) {
  lbl <- attr(x, "label")
  x <- factor(x, rev(levels(x)))
  attr(x, "label") <- lbl
  x
}




