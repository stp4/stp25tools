#' combine_data_frame
#'
#' @param ... data.frame
#' @param item default = 1 kann auch NULL sein
#' @param prefix names
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#'
#' m <- data.frame(
#'   Item = 1:3,
#'   a = (1:3),
#'   b = (1:3) * 2,
#'   c = (1:3) * 3
#' )
#' sd <- data.frame(
#'   Item = (1:3),
#'   a = (1:3) * 4,
#'   b = (1:3) * 5,
#'   c = (1:3) * 6
#' )
#' combine_data_frame(m, sd)
#' combine_data_frame(m, sd, item = NULL)
#' 
#' 
combine_data_frame <- function(..., by = 1, prefix = NULL) {
  tmp <- list(...)
  lng <-  lengths(tmp)
  if (length(unique(lng)) != 1L)
    stop("unequal input")
  if (!is.null(item)) {
    if (length(unique(sapply(tmp, "[", item))) != 1L)
      stop("unequal Items")
    measure <- seq_len(lng[1])[-item]
    rslt <- tmp[[1]][item]
  } else{
    measure <- seq_len(lng[1])
    rslt <- data.frame(row.names = seq_len(nrow(tmp[[1]])))
  }
  if (is.null(prefix))
    prefix <- as.character(substitute(list(...)))[-1]
  
  for (i in measure) {
    rslt <- cbind(
      rslt,
      as.data.frame(
        sapply(tmp, "[", i),
        col.names = paste(names(tmp[[1]])[i], prefix, sep = "_"),
        fix.empty.names = FALSE,
        stringsAsFactors = FALSE
      )
    )
  }
  rslt
}






#' Merge more then two Data Frames
#'
#' @param ... data.frames
#' @param by,by.x,by.y specifications of the columns used for merging
#' @param all,all.x,all.y logical
#' @param sort 		logical. Should the result be sorted on the by columns?
#' @param suffixes 	a character vector of length n 
#'
#' @return data.frame
#' @export
#'
#' @examples
#' 
#' #' set.seed(1)
#' n <- 10
#' df1 <- data.frame(
#'   origin = sample(c("A", "B", "C", "D", "E"), n, replace = T),
#'   N = sample(seq(9, 27, 0.5), n, replace = T),
#'   P = sample(seq(0.3, 4, 0.1), n, replace = T),
#'   C = sample(seq(400, 500, 1), n, replace = T)
#' )
#' df2 <-
#'   data.frame(
#'     origin = sample(c("A", "B", "C", "D", "E"), n, replace = T),
#'     foo1 = sample(c(T, F), n, replace = T),
#'     X = sample(seq(145600, 148300, 100), n, replace = T),
#'     Y = sample(seq(349800, 398600, 100), n, replace = T)
#'   )
#' 
#' 
#' df3 <-
#'   data.frame(origin = sample(c("A", "B", "C", "D", "E"), n, replace = T))
#' df4 <-
#'   data.frame(origin = sample(c("A", "B", "C", "D", "E"), n, replace = T))
#' 
#' rownames(df1) <- paste("P", sprintf("%02d", c(1:n)), sep = "")
#' rownames(df2) <- rownames(df1)
#' rownames(df3) <- rownames(df1)
#' rownames(df4) <- rownames(df1)
#' 
#' df1$id <- df2$id <- df3$id <- df4$id <-  rownames(df1)
#' merge(df1,
#'       df2,
#'       by = "id",
#'       all.x = F,
#'       all.y = F)
#' 
#' Merge2(df1, df2, df3, df4, by = "id")
#' 
#' 
Merge2 <-
  function (...,
            by = NULL ,
            by.x = by,
            by.y = by,
            all = FALSE,
            all.x = all,
            all.y = all,
            sort = TRUE,
            suffixes = NULL
            )
  {
    
    # stolen from
    # https://stackoverflow.com/questions/16666643/merging-more-than-2-dataframes-in-r-by-rownames
    #

    if(is.null(by))  stop(" by ... Fehlt! \n")
    data_list <-  list(...)
    i_suffixes <- 0:1
    
    if (is.null(suffixes))
      suffixes <-
      paste0(".",  letters[c(24, 25, 26, 21, 22, 23, 1:20)])[seq_len(length(data_list))]
    
    MyMerge <- function(x, y) {
      i_suffixes<<- i_suffixes+1 
      merge(
        x,
        y,
        by = by,
        by.x = by.x,
        by.y = by.y,
        all = all,
        all.x = all.x,
        all.y = all.y,
        sort = sort,
        suffixes = suffixes[i_suffixes]
      )
      
    }
    
    Reduce(MyMerge, data_list)
  }



