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
                                 c("freq", "mean", "median","multiresponse","multi", 
                                   "pie","dot", "hist", "box","bar")) {
  stringr::str_extract(tolower(x), paste0(search_string, collapse = "|"))
}

