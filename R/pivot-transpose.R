#' Dapply 
#' 
#' Dapply, dapply2 ist  plyr::llply()
#' 
#' @param x Objekt
#' @param ... Weitere Argumente an llply
#' @return  data.frame
#' @export
#' @examples
#'  
#' df1 <- Label(data.frame(
#'   month = rep(1:3, 2),
#'   student = rep(c("Amy", "Bob"), each = 3),
#'   A = c(9, 7, 6, 8, 6, 9),
#'   B = c(6, 7, 8, 5, 6, 7)
#' ),
#' A = "Deutsch",
#' B = "Mathe")
#' 
#' rs1 <- Dapply(~ A + B,
#'               df1,
#'               cut,
#'               breaks = 3,
#'               labels = c(1:3))
#'               
Dapply <- function(x, ...) {
  UseMethod("Dapply")
}





#' @rdname Dapply
#' @export
Dapply.formula <- function(x,
                           data,
                           fun = function(xx){as.numeric(xx)},
                           stringsAsFactors = default.stringsAsFactors(),
                           ...) {
  X <- prepare_data2(x, data)
  
  apply_data <- dapply2(X$data[X$measure.vars], 
                        fun=fun, 
                        stringsAsFactors=stringsAsFactors,
                        ...)
  
  data[, X$measure.vars] <- apply_data
  data
}


#' @rdname Dapply
#' @export
Dapply.data.frame <- function(data,
                              ...,
                              fun = function(x)
                                as.numeric(x),
                              stringsAsFactors = default.stringsAsFactors()) {
  
  X <- prepare_data2(data, ...)
  apply_data <- dapply2(X$data[X$measure.vars], fun)
  
  data[, X$measure.vars] <- apply_data
  data
  
}




#' @rdname Dapply
#' 
#' @description dapply2 = Copie of plyr::llply()
#' 
#' @param data  Data.frame
#' @param fun   funktion function(x) as.numeric(x)
#' @param stringsAsFactors logical: should character vectors be converted to factors?
#' @export
#' 
dapply2 <- function (data,
                     fun = function(x)
                       as.numeric(x),
                     stringsAsFactors = default.stringsAsFactors(),
                     ...) {
  if (tibble::is_tibble(data))
    set_label2(tibble::as_tibble(plyr::llply(data, fun, ...)),
               get_label2(data))
  else
    set_label2(data.frame(plyr::llply(data, fun, ...),
                          stringsAsFactors = stringsAsFactors),
               get_label2(data))
}





 

#' @param data an dplyr
#'
#' @param fun an dplyr default = as.numeric
#' @param stringsAsFactors an data.frame default = FALSE
#' @param ...
#'
#' @noRd
#'
dapply1 <-
  function (data,
            fun = function(x)
              as.numeric(x),
            stringsAsFactors = FALSE,
            ...) {
    if (tibble::is_tibble(data))
      tibble::as_tibble(plyr::llply(data, fun, ...))
    else
      data.frame(plyr::llply(data, fun, ...),
                 stringsAsFactors=stringsAsFactors)
  }





#' Transpose
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



#' @rdname transpose2
#' @export
transpose2.default <- function(x, ...) {
  do_transpose(x, ...)
}


#' @rdname transpose2
#' @export
transpose2.tbl_df <- function(x, ...) {
  tibble::as_tibble(do_transpose(x, ...))
  
}
#' @rdname transpose2
#' @export
transpose2.data.frame <- function(x, ...) {
  as.data.frame(do_transpose(x, ...))
  
}

#' @noRd
#' @param x data.frame
#'
#' @param key Item
#' @param value value
#'
#' @keywords internal
do_transpose <- function(x, key = "Item", value) {
  rslt <-  t(x)
  colnames(rslt) <- unlist(rslt[1, ])
  rslt <- rslt[-1, ]
  if (!is.null(row.names(rslt))) {
    rslt <-  cbind(my.row.names = row.names(rslt), rslt)
    colnames(rslt)[1] <- key
  }
  rslt
}






#' @rdname transpose2
#' @description  transpose_factor Rangreihe transortieren
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

transpose_factor <- function(x, levels = names(x)) {
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




