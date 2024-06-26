# combine_data_frame, Merge2, Rbind2


#' Merge/Combine 
#' 
#' Combine more then two Data Frames  Objects by Columns
#' 
#'
#' @param ... data.frames
#' @param sort 		logical. Should the result be sorted on the by columns?
#' @param suffixes 	a character vector of length n 
#' @param by,by.x,by.y id an merge
#' @param all,all.x,all.y  all an merge
#' @param include.label labels
#' @param include.rownames logical
#'
#' @return data.frame
#' @export
#'
#' @examples
#' 
#' #' set.seed(1)
#' n <- 10
#' df1 <- data.frame(
#'   origin = sample(c("A", "B", "C", "D", "E"), n, replace = TRUE),
#'   N = sample(seq(9, 27, 0.5), n, replace = TRUE),
#'   P = sample(seq(0.3, 4, 0.1), n, replace = TRUE),
#'   C = sample(seq(400, 500, 1), n, replace = TRUE)
#' )
#' df2 <-
#'   data.frame(
#'     origin = sample(c("A", "B", "C", "D", "E"), n, replace = TRUE),
#'     foo1 = sample(c(TRUE, FALSE), n, replace = TRUE),
#'     X = sample(seq(145600, 148300, 100), n, replace = TRUE),
#'     Y = sample(seq(349800, 398600, 100), n, replace = TRUE)
#'   )
#'
#'
#' df3 <-
#'   data.frame(origin = sample(c("A", "B", "C", "D", "E"), n, replace = TRUE))
#' df4 <-
#'   data.frame(origin = sample(c("A", "B", "C", "D", "E"), n, replace = TRUE))
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
#'       all.x = FALSE,
#'       all.y = FALSE)
#'
#' Merge2(df1, df2, df3, df4, by = "id")
#'
#'
#'
Merge2 <-
  function (...,
            by = 0 ,
            by.x = by,
            by.y = by,
            all = FALSE,
            all.x = all,
            all.y = all,
            sort = FALSE,
            suffixes = NULL,
            #include.units=FALSE,
            include.label = TRUE)  {
    # stolen from
    # https://stackoverflow.com/questions/16666643/merging-more-than-2-dataframes-in-r-by-rownames
    #
    # if (is.null(by))
    #  stop(" by ... Fehlt! \n")
    data_list <-  list(...)
    i_suffixes <- 0:1
    
    if (is.null(suffixes))
      suffixes <-
      paste0(".",  letters[c(24, 25, 26, 21, 22, 23, 1:20)])[seq_len(length(data_list))]
    
    MyMerge <- function(x, y) {
      xrwnm <- rownames(x)
      # xclnm <- colnames(x)
      yrwnm <- rownames(y)
      # yclnm <- colnames(y)
      
      if (tibble::is_tibble(x)) {
        x <- as.data.frame(x)
        #  colnames(x) <- xclnm
        rownames(x) <- xrwnm
      }
      if (tibble::is_tibble(y)) {
        y <- as.data.frame(y)
        # colnames(y) <- yclnm
        rownames(y) <- yrwnm
      }
      
      xrwnm <-   union(xrwnm,  yrwnm)
      
      i_suffixes <<- i_suffixes + 1
      rslt <- merge(
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
      
      if (by == 0)  {
        rownames(rslt) <- rslt[[1]]
        rslt[xrwnm,-1]
      }
      else
        rslt
      
    }
    
    
    if (include.label) {
      lvl <- NULL
      
      for (i in  seq_len(length(data_list))) {
        lv <- get_label(data_list[[i]],
                        include.units = FALSE)
        
        lvl <- c(lvl,
                 lv[setdiff(names(lv), names(lvl))])
      }
      # x<- c(a="A", b="B")
      # z<- c(a="A", c="C", d="D")
      # c(x, z[setdiff(names(z),  names(x))])
      set_label(Reduce(MyMerge, data_list),
                lvl)
      
    }
    else {
      Reduce(MyMerge, data_list)
    }
    
  }




# Merge2 <-
#   function (...,
#             by = NULL ,
#             by.x = by,
#             by.y = by,
#             all = FALSE,
#             all.x = all,
#             all.y = all,
#             sort = TRUE,
#             suffixes = NULL,
#             #include.units=FALSE,
#             include.label=TRUE)  {
#     # stolen from
#     # https://stackoverflow.com/questions/16666643/merging-more-than-2-dataframes-in-r-by-rownames
#     #
#     
#     if (is.null(by))
#       stop(" by ... Fehlt! \n")
#     data_list <-  list(...)
#     
#     
#     
#     i_suffixes <- 0:1
#     
#     if (is.null(suffixes))
#       suffixes <-
#       paste0(".",  letters[c(24, 25, 26, 21, 22, 23, 1:20)])[seq_len(length(data_list))]
#     
#     MyMerge <- function(x, y) {
#       i_suffixes <<- i_suffixes + 1
#       merge(
#         x,
#         y,
#         by = by,
#         by.x = by.x,
#         by.y = by.y,
#         all = all,
#         all.x = all.x,
#         all.y = all.y,
#         sort = sort,
#         suffixes = suffixes[i_suffixes]
#       )
#       
#     }
#     
#     
#     if (include.label) {
#       lvl <- NULL
#       
#       for (i in  seq_len(length(data_list))) {
#         lv <- get_label(data_list[[i]],
#                         include.units = FALSE)
#         
#         lvl <- c(lvl,
#                  lv[setdiff(names(lv), names(lvl))])
#       }
#       # x<- c(a="A", b="B")
#       # z<- c(a="A", c="C", d="D")
#       # c(x, z[setdiff(names(z),  names(x))])
#       set_label(
#         Reduce(MyMerge, data_list),
#         lvl
#       )
#       
#     }
#     else {
#       Reduce(MyMerge, data_list) 
#     }
#     
#   }


#' @rdname Merge2
#' @param prefix names
#'
#' @export
#'
#' @examples
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
#' combine_data_frame(m, sd, by = NULL)
#'
combine_data_frame <- function(..., 
                               by = 1, 
                               prefix = NULL) {
  # bis jetzt nur einmal verwendet
  
  tmp <- list(...)
  lng <-  lengths(tmp)
  if (length(unique(lng)) != 1L)
    stop("unequal input")
  if (!is.null(by)) {
    if (length(unique(sapply(tmp, "[", by))) != 1L)
      stop("unequal bys")
    measure <- seq_len(lng[1])[-by]
    rslt <- tmp[[1]][by]
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



