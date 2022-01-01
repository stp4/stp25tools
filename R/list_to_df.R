#' list_to_df
#'
#' @param x liste mit data.frames
#' @param name namen
#' @param last namen
#'
#' @return  tibbel
#' @export
#'
#' @examples
#'
#' x <- list(
#'   M1 = data.frame(
#'     Source = c("Intercept", "A", "B" , "C", "Residual"),
#'     b = c(0, 1, 2, 3, 0),
#'     y = c((1:4) + 10, 0)
#'   ),
#'
#'   M2 = data.frame(
#'     Source = c("Intercept", "A", "C", "B",  "D",  "E", "Residual"),
#'     x = c(0, 1, 3, 2, 4, 5, 0),
#'     y = c((1:6) + 21, 0)
#'   ),
#'   M3 = data.frame(
#'     Source = c("A", "B", "C", "D", "Residual"),
#'     x = c((1:4), 0),
#'     y = c((1:4) + 20, 0)
#'   ),
#'
#'   M1 = data.frame(
#'     Source = c("A", "B",  "D", "Residual"),
#'     x = c(1, 2, 4, 0),
#'     y = c((1:3) + 30, 0)
#'   )
#'
#' )
#'
#' list_to_df(x, last = "Residual")
#'
#' # A tibble: 7 x 9
#' # Source     M1_b  M1_y  M2_x  M2_y  M3_x  M3_y M1.1_x M1.1_y
#  # <chr>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>
#' #   1 Intercept     0    11     0    22    NA    NA     NA     NA
#' #   2 A             1    12     1    23     1    21      1     31
#' #   3 B             2    13     2    25     2    22      2     32
#' #   4 C             3    14     3    24     3    23     NA     NA
#' #   5 D            NA    NA     4    26     4    24      4     33
#' #   6 E            NA    NA     5    27    NA    NA     NA     NA
#' #   7 Residual      0     0     0     0     0     0      0      0
#'
list_to_df <- function(x,
                        name = make.names(gsub("_", "\\.", names(x)) , TRUE),
                        last = NULL) {
  res <- NULL
  names(x) <- name

  if(any( is.na( names(x)))) {
    warning("list without names!!!")
    names(x)<- make.names( names(x), unique = TRUE)
    }


  unique_names <-  unique(unlist(
                    lapply(x, function(xx) {
                    as.character(xx[[1]])})
                    ))

  if (!is.null(last))
    unique_names <-
    c(setdiff(unique_names, last),  last)


  pos <- seq_along(unique_names)
  names(pos) <- unique_names

 # cat("\n in list_to_df")

  for (i in names(x)) {
  #  cat("\n in for i=", i)
    df_i <- x[[i]]
    df_i[[1]] <-  as.character(df_i[[1]])

    which_name <- pos[-which(unique_names %in% df_i[[1]])]

    if (length(which_name > 0))
      df_i <- add_row_df(df_i, which_name)

    df_i <- df_i[match(unique_names, df_i[[1]]), ]

    names(df_i)[-1] <- paste0(i, "_", names(df_i)[-1])
    if (is.null(res)) {
      res <- df_i
    }
    else{
      res <- cbind(res, df_i[-1])
    }
  }
  #tibble::as_tibble(res)
  res

}

