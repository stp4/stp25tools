#stp25output

#' Transformiere zu data.frame
#'
#'
#' fix_levels ist fuer dta %>% gather(Parameter, Wert, x1:x5)  %>% fix_levels(dta)
#' fix_colnames ist fuer Formatieren in der Funktion \code{Output()}
#' @param x  vektor liste oder matrix
#' @param ... weiter Argumente
#' @return ein data.frame Objekt
#' @export
#'
#' @examples
#'
#' #'
#'    x <- list(
#' M1 = data.frame(
#'   Source = c("Intercept", "A", "B" , "C", "Residual"),
#'   b = c(0, 1, 2, 3, 0),
#'   y = c((1:4) + 10, 0)
#' ),
#'
#' M2 = data.frame(
#'   Source = c("Intercept", "A", "C", "B",  "D",  "E", "Residual"),
#'   x = c(0, 1, 3, 2, 4, 5, 0),
#'   y = c((1:6) + 21, 0)
#' ),
#' M3 = data.frame(
#'   Source = c("A", "B", "C", "D", "Residual"),
#'   x = c((1:4), 0),
#'   y = c((1:4) + 20, 0)
#' ),
#'
#' M1 = data.frame(
#'   Source = c("A", "B",  "D", "Residual"),
#'   x = c(1, 2, 4, 0),
#'   y = c((1:3) + 30, 0)
#' )
#'
#' )
#' dat <-   list(
#'   hoch = factor(c("A", "A", "A", "C", "C", "A"),   c("A", "B", "C", "D")),
#'   mittel = factor(c("B", "B", "B", "A", "B", "D"),   c("A", "B", "C", "D")),
#'   gering = factor(c("C", "C", "C", "B", "A", "B"),   c("A", "B", "C", "D"))
#' )
#'
#'
#' fix_to_df.list <- function(x, ...) {
#'   if (length(unique(lengths(x))) != 1)
#'     stop("Ungleiche Listen")
#'   if (all(sapply(x, function(z)
#'     is.vector(z) | is.factor(z))))
#'     as.data.frame(x)
#'   else
#'     list_to_df(x, ...)
#' }
#'
#' fix_to_df(x)
#'
#' fix_to_df(dat)
#'
#' fix_to_df(as.matrix(1:10))
#'
#'
#' mdat <- matrix(
#'   c(1, 2, 3, 11, 12, 13),
#'   nrow = 2,
#'   ncol = 3,
#'   byrow = TRUE,
#'   dimnames = list(c("row1", "row2"),
#'                   c("C.1", "C.2", "C.3"))
#' )
#' fix_to_df(mdat)
#' fix_to_df(table(rpois(100, 5)))
#'
#'

fix_to_df <- function(x, ...) {
  UseMethod("fix_to_df")
}

#' @rdname fix_to_df
#' @export
fix_to_df.default <- function(x, ...) {
  broom::tidy(x)
}


#' @rdname fix_to_df
#' @export
fix_to_tibble <- function(x, ...) {
  tibble::as_tibble(fix_to_df(x, ...))

}


#' @rdname fix_to_df
#' @export
fix_to_df.list <- function(x, ...) {
  if (length(unique(lengths(x))) != 1)
    stop("Ungleiche Listen")
  if (all(sapply(x, function(z)
    is.vector(z) | is.factor(z))))
    as.data.frame(x)
  else
    list_to_df(x, ...)
}



#' @rdname fix_to_df
#' @export
fix_to_df.data.frame <- function(x, ...) {
  x
}


#' @rdname fix_to_df
#' @param include.colnames columns as first rownames
#' @export
fix_to_df.matrix <-
  function(x, include.colnames = TRUE, ...) {
    if (include.colnames & (!is.null(rownames(x))))
      cbind(
        data.frame(Source = rownames(x),
                   stringsAsFactors = FALSE),
        as.data.frame(x, stringsAsFactors = FALSE)
      )
    else
      as.data.frame(x, stringsAsFactors = FALSE)
  }



#' @rdname fix_to_df
#' @export
fix_to_df.vector <- function(x, ...) {
  if (length(names(x)) == length(x))
    x <- as.data.frame(matrix(x,
                              nrow = 1,
                              dimnames = list("Source", names(x)))
                       , stringsAsFactors = FALSE)
  else
    data.frame(x = x)
}

#' @rdname fix_to_df
#' @param dim_x,atrb nicht zum aendern
#' @export
fix_to_df.ftable <-
  function(x,
           dim_x = dimension(x),
           atrb = attributes(x),
           ...) {
    if (dim_x > 1) {
      as.data.frame(as.data.frame.matrix(x, make.names = FALSE))
      rslt <-
        as.data.frame(as.data.frame.matrix(x, make.names = FALSE))
      names(rslt) <-
        paste(names(atrb$col.vars),   atrb$col.vars[[1]], sep = "_")
      dim_rslt <- dim(rslt)
      n<- ncol(rslt)

      for (i in   seq_along(atrb$row.vars)) {
        if ((i) < length(atrb$row.vars))
          rslt[names(atrb$row.vars)[[i]]] <-
            rep(atrb$row.vars[[i]], each = length(atrb$row.vars[[i + 1]]))
        else
          rslt[names(atrb$row.vars)[[i]]] <- atrb$row.vars[[i]]
      }
      rslt[c(seq_along(atrb$row.vars) + n, 1:n)]
    } else  if (dim_x < 2) {
      atrb <- dimnames(x)
      rslt <- as.data.frame(t(as.vector(x)))

      names(rslt) <- if (is.null(names(atrb))) atrb[[1]] else paste(names(atrb), atrb[[1]], sep = "_")
      rslt
    }



  }



#' @rdname fix_to_df
#' @export
#' @examples
#'
#'
#' data(infert, package = "datasets")
#' infert$case  <- factor(infert$case ,1:0, c("case", "control") )
#'
#' infert$spontaneous <- factor(infert$spontaneous)
#' infert$induced2    <- factor(infert$induced==0)
#' tab_1<- xtabs(~  case, infert)
#' tab_2x2<- xtabs(~ induced2 + case, infert)
#' tab_3x2<- xtabs(~ induced + case, infert)
#' tab_3x3<- xtabs(~ induced + education, infert)
#' tab_3x3x2<- xtabs(~ induced + education+case, infert)
#' tab_3x2x3<- xtabs(~ induced +case+ education, infert)
#' fix_to_df(tab_1)
#'  # 2x2
#' tab_2x2
#' fix_to_df(tab_2x2)
#' #fix_to_df(ftable(tab_2x2))
#'  # 3x2
#' tab_3x2
#' fix_to_df(tab_3x2)
#' #fix_to_df(ftable(tab_3x2))
#'  # 3x3
#' tab_3x3
#' fix_to_df(tab_3x3)
#' #fix_to_df(ftable(tab_3x3))
#'  # 3x3x2
#' tab_3x3x2
#' fix_to_df(tab_3x3x2)
#' #fix_to_df(ftable(tab_3x3x2))
#'
#'  # 3x2x3
#' tab_3x2x3
#' # ftable(tab_3x2x3)
#' # as.data.frame(ftable(tab_3x2x3)) %>%
#' #   stp25aggregate::Wide(induced  +  case ~education )
#' fix_to_df(tab_3x2x3)
#'
fix_to_df.table <- function(x, dim_x = dimension(x), ...) {
  if (dim_x  > 1)
    fix_to_df.ftable(stats::ftable(x), dim_x=dim_x, ...)
  else
    fix_to_df.ftable(x, dim_x=dim_x, ...)
}

dimension <- function(x) {
  dm <- dim(x)
  ldm <-  length(dm)
  if (ldm == 1) 1
  else if (ldm == 2 & prod((dm - 1)) == 1) 2
  else if (ldm == 2) 3
  else ldm+1
}





