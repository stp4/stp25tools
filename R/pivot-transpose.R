#' @rdname Dapply
#' @description  transpose2: Data Frame Transpose
#' 
#' Alternative: data.table::transpose(l, make.names = 1, keep.names = "Item")
#'
#' @param x Objekt (Data.Frame oder Tibble)
#' @param ...   key="Item"
#'
#' @return gleiches Objekt
#' @export
#'
#' @examples
#' 
#' 
#' transpose2(data.frame(pos = c("A", "B"), x = 1:2))
#' 
#' transpose2(data.frame(
#'   pos = c("A", "B" , "C"),
#'   x = 1:3,
#'   y = 3:5
#' ))
transpose2 <- function(x, ...) {
  UseMethod("transpose2")
}



#' @rdname Dapply
#' @export
transpose2.default <- function(x, ...) {
  do_transpose(x, ...)
}


#' @rdname Dapply
#' @export
transpose2.tbl_df <- function(x, ...) {
  tibble::as_tibble(do_transpose(x, ...))
  
}

#' @rdname Dapply
#' @export
transpose2.data.frame <- function(x, ...) {
  as.data.frame(do_transpose(x, ...))
}

#' @noRd
#' @param x data.frame
#'
#' @param key  name of the first column Item
#' @param col.names,row.names The name  of the  column  and rows
#'
#' @keywords internal
do_transpose <- function(x, 
                         key = "Item", 
                         col.names = NULL,
                         row.names = NULL) {
  rslt <- t(x)
  if(is.null(col.names)) col.names <- unlist(rslt[1, ])
  else{
    if((ncol(rslt)+1) == length(col.names)) {
      key<- col.names[1]
      col.names <- col.names[-1]
    }
  } 
  colnames(rslt) <- col.names
  
  rslt <- rslt[-1, ]
  
  if(is.null(row.names)) row.names <- row.names(rslt)
  
  if (!is.null(row.names(rslt))) {
    rslt <-  cbind(my.row.names = row.names, rslt)
    colnames(rslt)[1] <- key
  }
  rslt
}


#' @rdname Dapply
#' @description  transpose_factor: Rangreihe transortieren
#'
#' @param x data.frame
#' @param levels optional Levels
#'
#' @return data.frame
#' @export
#' @examples
#'
#'    dat <-   data.frame(
#'   hoch = factor(c("A", "A", "A", "C", "C", "A"),   c("A", "B", "C", "D")),
#'   mittel = factor(c("B", "B", "B", "A", "B", "D"),   c("A", "B", "C", "D")),
#'   gering = factor(c("C", "C", "C", "B", "A", "B"),   c("A", "B", "C", "D"))
#'   )
#'   transpose_factor(dat, NULL)
#'   transpose_factor(dat)
#' #'
#'
#'    dat <-   data.frame(
#'   hoch = factor(c("A", "A", "A", "C", "C", "A"),   c("A", "B", "C", "D")),
#'   mittel = factor(c("B", "B", "B", "A", "B", "D"),   c("A", "B", "C", "D")),
#'   gering = factor(c("C", "C", "C", "B", "A", "B"),   c("A", "B", "C", "D"))
#'   )
#'   transpose_factor(dat, NULL)
#'   transpose_factor(dat)

transpose_factor <- function(x, 
                             levels = names(x)) {
  lvl = levels(x[[1]])
  transposed <- t(apply(x, 1, function(z) {
    trans <- NULL
    for (i in lvl) {
      tr <- which(z == i)
      if (length(tr) == 0)
        tr <- 0
      names(tr) <- i
      trans <- c(trans, tr)
    }
    trans
  }))
  if (is.null(levels))
    as.data.frame(transposed)
  else
    as.data.frame(lapply(
      as.data.frame(transposed),
      factor, seq_along(levels), levels))
}




