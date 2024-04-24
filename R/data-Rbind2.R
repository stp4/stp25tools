#' @rdname Merge2
#' @description Rbind2: dplyr::bind_rows() mit labels + name der Daten
#' @param .names alternative zur vergabe der labels in which
#' @param .id Data frame identifier.  dplyr::bind_rows(..., .id = NULL)
#' @param .use.label set_label TRUE/FALSE
#' @return data.frame(which, ...)
#' @export
#'
#' @examples
#'
#'
#' # require(plyr)
#' # require(stp25tools)
#'
#' df1 <- data.frame(a=1, b=2)
#' df2 <- data.frame(a=2, c=3, d=5)
#'
#'
#'
#' do.call(plyr::rbind.fill, list(df1, df2))
#' Rbind2(df1, df2)
#'
#'
#' df1 = data.frame(CustomerId = c(1:6),
#' Product = c(rep("Oven", 3), rep("Television", 3)))
#'
#' df2 = data.frame(CustomerId = c(4:7),
#' Product = c(rep("Television", 2), rep("Air conditioner", 2)))
#'
#' df3 = data.frame(
#'   CustomerId = c(4:7),
#'   Product = c(rep("Television", 2), rep("Air conditioner", 2)),
#'   State = c(rep("California", 2), rep("New Jersey", 2))
#' )
#'
#' Rbind2(df1, df3)
#'
#' dplyr::bind_rows(df1, df2)
#'
Rbind2 <- function (...,
                    .id = "which",
                    .names = NULL,
                    .use.label = TRUE,
                    include.rownames = FALSE) {
  
  data <- dplyr::bind_rows(..., .id = .id)
  
  if (include.rownames)   {
    data <- cbind(data[1],
                  Source =  sub("(.*).....*", "\\1", rownames(data)),
                  data[-1])
  }
  
  if (!is.null(.id)) {
    tmp <- list(...)
    if (is.null(.names))
      .names <- names(tmp)
    
    if (is.null(.names))
      .names <- sapply(as.list(match.call()), deparse)[-1]
    
    data[[1]] <-
      as.character(factor(data[[1]], seq_along(.names), .names))
  }
  
  data[[1]] <- factor(data[[1]])
  
  if (.use.label) {
    label <- c(.id)
    names(label) <- .id
    for (dat in list(...)) {
      lbl <-  get_label2(dat)
      label <-
        append(label, lbl[setdiff(names(lbl), names(label))])
      data <- set_label2(data, label)
    }
  }
  
  data
}

#' @rdname Merge2
#' @param list  Spalten sie aufgedröselt werden list(t0 = 1:3, t1 = 4:6, t2 = 7:9)
#' @description
#' Rbind3 Combinirt Zeilenweise aber es wird nur ein data.frame übergeben der anhand einer liste aufgesplittet wird
#' 
#' @export
Rbind3 <-
  function(.data,
           list = list(NULL),
           .id = "time",
           names = NULL,
           ...) {
    new_data <- NULL
    times <- names(list)
    if (is.null(names))
      names <- names(.data[list[[1]]])
    
    for (i in times) {
      new_data_i <- tibble::as_tibble(cbind(.id = i, .data[list[[i]]]))
      names(new_data_i) <- c(.id, names)
      if (is.null(new_data))
        new_data <- new_data_i
      else
        new_data <- rbind(new_data, new_data_i)
      
    }
    new_data
  }


# require(stp25tools)
# require(tidyverse)
# df1 <-
#   data.frame(CustomerId = c(1:6), Product = c(rep("Oven", 3), rep("Television", 3))) |>
#   Label(Product = "Produkt")
# df2 <-
#   data.frame(CustomerId = c(4:7), Product = c(rep("Television", 2), rep("Air conditioner", 2)))
# df3 <- data.frame(
#   CustomerId = c(4:7),
#   Product = c(rep("Television", 2), rep("Air conditioner", 2)),
#   State = c(rep("California", 2), rep("New Jersey", 2))
# ) |>
#   Label(Product = "Produkt-Kategorie", State = "Bundes-Staat")
# 
# Rbind2(df1, df2, df3, .id = NULL, .use.label = FALSE)
