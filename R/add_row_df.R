# add_row_df





#' add_row_df
#'
#' Add row to data.frame.
#'
#' @param x data.frame
#' @param add_row  named vector
#' @param pos an rbind_at  
#'
#' @return data.frame
#' @export
#'
#' @examples
#' df <-   data.frame(
#' Source = c("A", "B", "C", "F"),
#' x = 1:4,
#' y = 1:4,
#' stringsAsFactors = FALSE
#' )
#'
#' add_row_df(df, c("Erste Zeile" = 1, "Dritte" = 3))
#'
#' add_row_df(df, "Erste Zeile")
#' add_row_df(df, c("Erste Zeile", "Zweite"))
#' add_row_df(df, c("Erste Zeile" = 1, "letzte" = 5))
#'
#'
#'
add_row_df <- function(x, add_row = NULL, pos=NULL) {
 # cat("\n in add_row_df")
  if (length(add_row) > 0 & is.null(names(add_row))) {

    nms <- add_row
    add_row <- seq_len(length(add_row))
    names(add_row) <- nms
  }
  new_element <- add_emty_col(x, names(add_row))
  if(is.null(pos)) pos <-  as.numeric(add_row)
  rbind_at(x, new_element, pos = pos)
}



add_emty_col <- function (x,  df = "hallo welt", na_value = NA)
{
  df <- tibble::tibble(df)
  names(df) <-  names(x)[1L]
  attr(df, "row.names") <- .set_row_names(max(1L, nrow(df)))

  missing_vars <- setdiff(names(x), names(df))
  df[missing_vars] <- na_value
  df <- df[names(x)]
  df
}


#' Orginal  tibble (rbind_at)
#'
#' @noRd
#'
rbind_at <- function (old, new, pos)
{
  idx <- NULL
  if( any(is.na(pos))) pos[which(is.na(pos))] <- 1

  if (nrow(old) == 0) {
    old <- old[1, ]
    out <- rbind(old, new)[-1, ]
  }
  else {
    out <- rbind(old, new)
    pos_old <- seq_len(nrow(old))
    pos_new <- seq_len(length(new)) + nrow(old)
    for (i in pos_old) {
      if (any(i == pos)) {
        idx <- c(idx, pos_new[which(i == pos)], i)
      }
      else{
        idx <- c(idx, i)
      }
    }

    if (length(idx) < nrow(out)) {
      idx <- c(idx, seq(max(idx, na.rm=TRUE) + 1, nrow(out)))
    }
    out <- out[idx, ]
  }
  out
}
