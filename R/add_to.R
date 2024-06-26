# add_to



#' add_to
#'
#' @param x Dataframe ore list
#' @param ... what to add
#'
#' @return  data.frame ore list
#' @export
#'
#' @examples
#' 
#' #' Eine oder mehere Zeilen zu einem data.fram hinzufügen
#' 
#' df <-   data.frame(
#'   Source = c("A", "B", "C", "F"),
#'   x = 1:4,
#'   y = 1:4,
#'   stringsAsFactors = FALSE
#' )
#' 
#' df
#' add_to(df, "Erste Zeile" = 1, "Dritte" = 3)
#' add_to(df, "Erste Zeile" = 1, "letzte" = -1)
#' add_to(df, list("G", 5), pos = -1)
#' add_to(df, data.frame(Source = c("G", "H"), x = 5:6), pos = -1)
#'
add_to <-function(x, ...){
  UseMethod("add_to")
}


#' @rdname add_to
#' @export
#' 
#' @examples
#' 
#' # add something to list
#'  
#' my_setting <- list(
#'   a = list(alpha = 1, col = "2"),
#'   b = list(alpha = 1, col = "2"),
#'   c = list(alpha = 1, col = "2")
#' )
#' my_setting <- add_to(my_setting,
#'                     c = list(fill = "gray80"),
#'                     b = list(alpha = 2, beta = 3))
#' my_setting$c
#' 
add_to.list <- function(x, ...) {
  what <- list(...)
  if( length(what)==1 & is.list(what[[1]]))# what <- what[[1]] # geändert aber noch nicht getestet
    what <- what[1]
  utils::modifyList(x, what)
}


#' @rdname add_to
#' @export
#' @examples 
#'  # tibble::add_column(.data )
add_to.data.frame <- function(x, ...) {
  what <- list(...)
 # print(is.list(what[[1]]))

  if (is.list(what[[1]])) {
    new <- fix_to_df(what[[1]])
    if (ncol(new) < ncol(x))
      new <- cbind(
        new,
        matrix(NA, ncol = ncol(x) - ncol(new),
               nrow = nrow(new)))

    names(new)<- names(x)
    if (is.null(what[[2]])) pos <- 1
    else pos <- as.numeric(what[[2]])

    rslt <- rbind_at(x, new,  pos = pos)

  }
  else{
    if( length(what)==1) what<- what[[1]]
    rslt <- add_row_df(x, add_row = what)
  }
  if (tibble::is_tibble(x))
    rslt <- tibble::as_tibble(add_row_df(x, add_row = what))

  rslt
}
