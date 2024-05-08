#' Imputation of LOD
#'
#' Limit Of Detection (LOD)  is the minimum detectable value. (blank signal + 3SD)
#'
#' Replace NA by LOD*threshold and
#' round to the next order of magnitude
#'
#' https://github.com/jranke/chemCal
#'
#' @param x numeric  vector to round
#' @param lod  Limit of detection if NULL lod = min - 3sd
#' @param threshold default 1/sqrt(2) replace NA and x < lod by lod * threshold
#' @param na.replace should NA be replaced by the lod  * threshold
#' @param force.lod should all values smaller than the LOD be replaced?
#' @param accuracy  significant digits for rounding
#'
#' @return vector, data.frame
#' @export
#'
#' @examples
#'
#'  x <-
#' c(.00049001,.0035648,.01,.0112,
#'   .023212548,.00541257,.004041257,.458,.500)
#' y <-
#'   c(43.01,49.156,678.00112458964,789.023212548,
#'     674.00049001,634.00541257,76.004041257,789.458,500
#'   )
#'
#' data.frame(x = signif(x, 3), x.lod = imputation_LOD(x,  lod = .0035648))
#'
#' data.frame(y = signif(y, 3), y.lod = imputation_LOD(y,  lod = 49.156))
imputation_LOD <- function(x, ...) {
  UseMethod("imputation_LOD")
}
#' @rdname imputation_LOD
#' @export
imputation_LOD.default <-
  function(x,
           lod = NULL,
           threshold =  .0707,
           # 1/sqrt(2)
           na.replace = TRUE,
           force.lod = TRUE,
           accuracy = 1) {
    # LOD aus den Daten ableiten
    if (is.null(lod))
      lod <- min(x, na.rm = TRUE) - 3 * sd(x, na.rm = TRUE)
    # Messwert gilt häufig als quantitativ (bestimmt), wenn die Genauigkeit der
    # Messung um den Faktor 3.33 höher (besser) ist als die Genauigkeit der Nachweisgrenze.
    # loq <- 3.33 * lod
    
    # Anzahl an digits fuer die accuracy bestimmen
    if (abs(lod) < 1)
      dgt <-
        as.integer(log10(abs(lod) - floor(abs(lod))) * (-1) + accuracy)
    else
      dgt <- as.integer(log10(round(abs(lod))) * (-1) + accuracy)
    
    if (force.lod)
      x[x < lod] <- lod * threshold
    
    if (na.replace)
      x[is.na(x)] <- lod * threshold
    
    # cat("digits =", dgt, ", LOD =", round(lod, dgt + 1), "\n")
    attr(x, "LOD") <- round(lod, dgt)
    
    if (dgt < 1)
      round(round(x), dgt)
    else
      round(x, dgt)
  }

#' @rdname imputation_LOD
#' @param remove.lod NULL or fraction
#' @export
#' @examples
#'
#'
#' #' #require(stp25tools)
#'
#' DF <-
#'   data.frame(x=rnorm(10), y=rnorm(10), z=rnorm(10)) |>
#'   Label( x ="Asp", y ="Trp", z ="Leu")
#' DF[1,1] <- NA
#'
#' imputation_LOD(DF, lod =c(-.2, .2, .05), accuracy=3)
#' imputation_LOD(DF, lod = c(-.2, .2, .05), accuracy = 3, na.replace =FALSE)
imputation_LOD.data.frame <-
  function(x,
           lod = NULL,
           threshold = .0707,
           # 1/sqrt(2)
           na.replace = TRUE,
           force.lod = TRUE,
           accuracy = 1,
           remove.lod = 1 / 2) {
    # test input
    my <- names(x)
    
    has_names <- !is.null(names(lod))
    if (has_names) {
      if (is.data.frame(lod)) {
        lod_t <- t(lod)
        lod <- as.vector(lod_t[, 1])
        names(lod) <- row.names(lod_t)
      }
      if (all(my %in% names(lod))) {
        lod <-  lod[my]
      }
      else {
        my <- my[which((my %in% names(lod)))]
        lod <- lod[my]
      }
    } else {
      if (length(my) != length(lod))
        stop("Wenn LOD ohne Namen übergeben werden müssen diese gleich lang sein wie die Daten!")
    }
    
    if (any(lod < 0))
      stop("Negative LOD machen Physikalisch keinen Sinn.")
    res <- purrr::map2(
      x[my],
      lod,
      \(y, lod) imputation_LOD.default(
        y,
        lod,
        threshold = threshold,
        na.replace = na.replace,
        force.lod = force.lod,
        accuracy = accuracy
      )
    )
    
    
    res <- dplyr::bind_rows(res, .id = .id)
    x[my] <- res[my]
    
    if (!is.null(remove.lod)) {
      w_remove.lod <-
        mapply(function(x, y) {
          length(x) * remove.lod   <  sum(x < y, na.rm = TRUE)
        }, x[my], lod)
      
      if (any(w_remove.lod)) {
        w_remove.lod <- names(w_remove.lod)[w_remove.lod]
        cat("\n I will remove folowing Items! (LOD Limit ist " ,remove.lod*100, "%) \n")
        print(w_remove.lod)
        x[w_remove.lod] <- NULL
      }
    }
    
    
    
    x
  }



# DF <-
#   data.frame(
#     x = rnorm(10),
#     y = rnorm(10),
#     z = rnorm(10),
#     f = 1:10
#   ) |>
#   Label(x = "xAsp", y = "yTrp", z = "zLeu")
# DF[1:6, 1] <- NA
# DF[2, 2] <- NA
# DF[3, 3] <- NA
# DF[4, 4] <- NA
# 
# #imputation_LOD(DF, lod = data.frame(z=  .3,  a=.2, y = .05), accuracy = 3, na.replace =FALSE)
# 
# 
# x <- imputation_LOD(DF,
#                     lod = c(
#                       z = 5,
#                       x = .21111,
#                       y = .2,
#                       f = .05
#                     ),
#                     accuracy = 3)
# x
# #imputation_LOD(DF, lod = data.frame(z= -.2,  a=.2, y = .05), accuracy = 2)
