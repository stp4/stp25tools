#' @importFrom stp25settings default_stp25
#' @export
stp25settings::default_stp25


#' @importFrom stp25settings get_opt
#' @export
stp25settings::get_opt

#' @importFrom stp25settings set_opt
#' @export
stp25settings::set_opt


#' stp25_stat_methode
#'  
#' in Tbll verwendet
#' 
#' @param x string
#' @param search_string Meine Methoden
#' @noRd
stp25_stat_methode <- function(x,
                               search_string =
                                 c("freq",
                                   "mean",
                                   "median",
                                   "multiresponse",
                                   "multi",
                                   "ratio",
                                   "pie",
                                   "dot",
                                   "hist",
                                   "box",
                                   "bar")) {
  stringr::str_extract(tolower(x), paste0(search_string, collapse = "|"))
}



# reshape2 ist ein altes packages und womöglich bald osolet


#' Split a vector into multiple columns
#' 
#' Stolen from reshape2
#'
#' @param string character vector or factor to split up
#' @param pattern regular expression to split on
#' @param names names for output columns
#'
#' @examples
#' x <- c("a_1", "a_2", "b_2", "c_3")
#' vars <- reshape2_colsplit(x, "_", c("trt", "time"))
#' vars
#' str(vars)
reshape2_colsplit<-
function (string, pattern, names) 
{
  vars <- stringr::str_split_fixed(string, pattern, n = length(names))
  df <- data.frame(plyr::alply(vars, 2, type.convert, as.is = TRUE), 
                   stringsAsFactors = FALSE)
  names(df) <- names
  df
}

#reshape2:::guess_value
guess_value <- function (df){
  if ("value" %in% names(df))
    return("value")
  
  last <- names(df)[ncol(df)]
  message("Using ", last, " as value column: use value to override.")
  last
}



#' Skewness
#' 
#' stolen from e1071::skewness
#' 
#' Type 1:
#' g_1 = m_3 / m_2^{3/2}  This is the typical definition used in many older textbooks.
#' 
#' Type 2:
#'   G_1 = g_1 sqrt{n(n-1)} / (n-2)  Used in SAS and SPSS.
#' 
#' Type 3:
#'   b_1 = m_3 / s^3 = g_1 ((n-1)/n)^{3/2}  Used in MINITAB and BMDP.
#'
#' @param x x	a numeric vector containing the values whose skewness is to be computed.
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @param type an integer between 1 and 3 selecting one of the algorithms for computing skewness detailed below.
#'
#' @examples
#' x <- rnorm(100)
#' skewness(x)
e1071_skewness <- 
function (x, na.rm = FALSE, type = 3) 
{
  if (any(ina <- is.na(x))) {
    if (na.rm) 
      x <- x[!ina]
    else return(NA)
  }
  if (!(type %in% (1:3))) 
    stop("Invalid 'type' argument.")
  n <- length(x)
  x <- x - mean(x)
  y <- sqrt(n) * sum(x^3)/(sum(x^2)^(3/2))
  if (type == 2) {
    if (n < 3) 
      stop("Need at least 3 complete observations.")
    y <- y * sqrt(n * (n - 1))/(n - 2)
  }
  else if (type == 3) 
    y <- y * ((1 - 1/n))^(3/2)
  y
}

