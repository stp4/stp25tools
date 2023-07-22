#' Summen-Index
#'  
#'  Summen Index eine Summenfunktion mit der Erweiterung zum Umcodieren
#' @return Vektor
#' @export
#' 
Index <- function(x, ...) {
  UseMethod("Index")
}
Index.formula <- function(x,
                          data,
                          # key = "variable",
                          # value = "value",
                          ...) {
  Index.data.frame(data[ all.vars(x) ])
}

#' @rdname Index
#' @export
Index.data.frame <- function(x,
                             ...,
                             re.code = FALSE,
                             fun = "mean",
                             na.rm = TRUE,
                             digits = 4,
                             max.level = NA,
                             min.level = NA) {
  
  dots <-
    sapply(lazyeval::lazy_dots(...), function(x) as.character(x[1]))
  if(length(dots)>0) x <- x[dots]
  
  if (!all(apply(x, 2, function(objekt) {
    class(objekt) == "numeric" || class(objekt) == "integer"
  }))) {
    if (any(unlist(
      lapply(x, function(objekt)
        class(objekt) == "factor" ||
        class(objekt) == "labelled")
    ))) {
      cat("\nKonvertiere Faktoren zu Zahlen!\n\n")
      x <- data.frame(lapply(x, as.numeric))
    } else {
      cat(
        "\n",
        "Falsches Datenformat (Numeric oder Faktor ist erlaubt)",
        "\n",
        apply(x, 2, function(objekt)
          class(objekt)),
        "\n\n"
      )
      return(rep(NA, nrow(x)))
    }
  }
  if (!is.logical(re.code)) {
    cat("\n", "Umcodieren ", paste(re.code, collapse = ", "), "\n")
    print(head(x))
    x <- Umcodieren(x, re.code, max.level, min.level)
    print(head(x))
  } else if( re.code ) {
    
    cat("\n", "Prüfe keys mit psych" , "\n") 
    alp_check <-
      psych::alpha(x, check.keys = TRUE, warnings = FALSE)
    keys <- unlist(alp_check$keys)
    keys <-   which( ifelse(grepl("\\-", keys), -1, 1) == -1 )
     print(keys)
    if( length(keys)>0){
      cat("\n", "Umcodieren mit psych" , "\n")  
      x <- Umcodieren(x, keys, max.level, min.level)
    }

   
  }
  index <- switch(
    fun,
    mean = round(rowMeans(x, na.rm = na.rm), digits),
    sum =  round(rowSums(x, na.rm = na.rm), digits),
    rep(NA, nrow(x))
  )
  index
}


#' @rdname Index
#' @export
Index.default <- function( ...,
                           re.code = FALSE,
                           fun = "mean",
                           na.rm = TRUE,
                           digits = 4,
                           max.level = NA,
                           min.level = NA ) {
  
  
  dots<-  stp25tools::fix_to_df(list(...))
  Index.data.frame(dots,   
                   re.code=re.code,
                   fun=fun,
                   na.rm=na.rm,
                   digits=digits,
                   max.level=max.level,
                   min.level=min.level )
  
  
}


#' @rdname Index
#' @export
Sum2 <- function(...,
                 re.code = FALSE,
                 fun = "mean",
                 na.rm = TRUE,
                 digits = 4,
                 max.level = NA,
                 min.level = NA) {
  dat <- stp25tools::fix_to_df(list(...))
  Index(dat, re.code =re.code, 
        fun =fun,
        na.rm =na.rm,
        digits=digits,
        max.level=max.level,
        min.level=min.level)
  
}


#' Umcodieren
#'
#' @noRd
Umcodieren <- function(x,
                       re.code,
                       max.level = NA,
                       min.level = NA) {
  if (is.na(max.level))
    max.level <- max(x, na.rm = TRUE)
  if (is.na(min.level))
    min.level <- min(x, na.rm = TRUE)
  mytempdata <- x[, re.code]
  
  if (is.numeric(mytempdata))
    x[, re.code] <- max.level + min.level - mytempdata
  else
    x[, re.code] <-
    apply(mytempdata, 2, function(item)
      max.level + min.level - item)
  return(x)
}



# Transformieren zu numeric
#
# @noRd
#transform_to_numeric2 <- function(data, data_range) {
#   #data2<- na.omit(data)
#   lvls <- get_label(data)
#   
#   objects <-
#     sapply(data, function(x)
#       if (is.factor(x))
#         "factor"
#       else if (is.numeric(x))
#         "numeric"
#       else
#         "unknown")
#   if (all(objects == "numeric"))
#     data_range <- range(data, na.rm = T)
#   else if (all(objects == "factor")) {
#     data <- data.frame(sapply(data, as.numeric))
#     data_range <- range(data, na.rm = T)
#   }
#   else {
#     cat("\n",
#         "Falsches Datenformat (Numeric oder Faktor ist erlaubt)",
#         "\n")
#     # print(objects)
#     data <- sapply(data, as.numeric)
#     data_range <- range(data, na.rm = T)
#   }
#   
#   list(data = data,
#        range = data_range,
#        labels = lvls)
#   
# }

# head(DF[Cs(erfahrung.1,
#            erfahrung.2,
#            erfahrung.3,
#            erfahrung.4)])
# Index(DF[Cs(erfahrung.1,
#             erfahrung.2,
#             erfahrung.3,
#             erfahrung.4)])
# DF$erfahrung.5 <- rev( DF$erfahrung.4)
# Index(DF,
#       erfahrung.1,
#       erfahrung.2,
#       erfahrung.3,
#       erfahrung.5,
#       re.code=TRUE)
# 
# DF2<- DF |> mutate(erfahrung=Index(
#   erfahrung.1,
#   erfahrung.2,
#   erfahrung.3,
#   erfahrung.5,
#   re.code=TRUE))
# DF2$erfahrung
