#stp25stat:::transpose3

#' @param .data an dplyr
#'
#' @param fun an dplyr default = as.numeric
#' @param stringsAsFactors an data.frame default = FALSE
#' @param ...
#'
#' @noRd
#'
dapply1 <-
  function (.data,
            fun = function(x)
              as.numeric(x),
            stringsAsFactors = FALSE,
            ...) {
    if (tibble::is_tibble(.data))
      tibble::as_tibble(plyr::llply(.data, fun, ...))
    else
      data.frame(plyr::llply(.data, fun, ...),
                 stringsAsFactors=stringsAsFactors)
  }


#' Rangreihe transortieren
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




