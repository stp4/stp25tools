#' Creation of dummy variables and reverse
#'
#' @param x factor
#' @return data.frame
#' @export
factor_to_dummy <- function(x) {
  ans <- list()
  for (i in levels(x))
    ans[[i]] <- ifelse(x == i, 1L, 0L)
  
  as.data.frame(ans)
}



#' @rdname factor_to_dummy
#' @param x data.frame all 0 / 1
#'
#' @return factor
#' @export
#'
#' @examples
#' 
#'  z <- gl(3, 2, 12, labels = c("apple", "salad", "orange"))
#' table(z)
#' levels(z) <- list("veg"   = "salad", "fruit" = c("apple", "orange"))
#' table(z)
#' z <- factor_to_dummy(z)
#' table(z)
#' z <- dummy_to_factor(z)
#' table(z)
dummy_to_factor <- function(x, value=1L) {
  lbl <-  get_label(x)
  x[is.na(x)] <- 0L
 
  for (i in seq_along(x)) {
    x[[i]] <- ifelse(x[[i]] == value, i, 0L)
  }
  factor(rowSums(x), seq_along(x), lbl)
}

