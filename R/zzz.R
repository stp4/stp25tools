#' @keywords internal
#' @importFrom plyr llply alply
#' @importFrom dplyr select filter n_distinct bind_rows mutate bind_rows
#' @importFrom tibble is_tibble as_tibble tibble
#' @importFrom tidyr pivot_longer pivot_wider gather uncount separate
#' @importFrom lazyeval lazy_dots 
#' @importFrom rlang enquo quo_get_expr quo_get_expr is_formula
#' @importFrom stringr str_split str_extract str_split_fixed str_replace_all
#' @importFrom Formula Formula
#' @importFrom writexl write_xlsx
#' @importFrom readxl read_excel
#' @importFrom utils write.csv2 modifyList
#' @importFrom haven read_sav as_factor 
#' @importFrom tools file_ext
#' @importFrom readr parse_number
#' @importFrom tools file_ext
#' @importFrom stp25settings get_opt
# @importFrom magrittr %>%
#' @importFrom stats as.formula formula na.exclude na.omit na.pass relevel reorder update
#' @importFrom utils head read.table type.convert
#' @importFrom Hmisc all.is.numeric
#' @importFrom purrr map2

#' @importFrom Hmisc cut2
#' @export
Hmisc::cut2
 
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
#' e1071::skewness(x)
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

