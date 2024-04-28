#https://stackoverflow.com/questions/35553244/count-leading-zeros-between-the-decimal-point-and-first-nonzero-digit


#' Round to the next order of magnitude
#'
#' @param x numeric  vector to round
#' @param lod Limit of dedection (accuracy)
#' @param threshold replace NA and x < lod by lod * threshold
#' @param na.replace should NA be replaced by the lod  * threshold
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
#'
#' data.frame(x = signif(x, 3), x.lod = round_lod(x,  lod = .0035648))
#'
#' data.frame(y = signif(y, 3), y.lod = round_lod(y,  lod = 49.156))

round_lod <- function(x, ...) {
  UseMethod("round_lod")
}
#' @rdname round_lod
#' @export
round_lod.default <-
  function(x,
           lod = NULL,
           threshold = .5,
           na.replace = TRUE,
           accuracy = 1) {
    if (is.null(lod))
      lod <- min(x, na.rm = TRUE)
    
    if (abs(lod) < 1)
      dgt <-
        as.integer(log10(abs(lod) - floor(abs(lod))) * (-1) + accuracy)
    else
      dgt <- as.integer(log10(round(abs(lod))) * (-1) + accuracy)
    
    x[x < lod] <- NA
    if (na.replace)
      x[is.na(x)] <- lod * threshold
    
    cat(" -> digits =", dgt, ", LOD =", round(lod, dgt + 1), "\n")
    
  
   attr(x,  "LOD")  <- round(lod, dgt + 1)
      print( attributes(x))
      
      
    if (dgt < 1)
      round(round(x), dgt)
    else
      round(x, dgt)
  }

#' @rdname round_lod
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
#' round_lod(DF, lod =c(-.2, .2, .05), accuracy=3)
#' round_lod(DF, lod = c(-.2, .2, .05), accuracy = 3, na.replace =FALSE)
round_lod.data.frame <-
  function(x,
           lod = NULL,
           threshold = .5,
           na.replace = TRUE,
           accuracy = 1) {
    res <- purrr::map2(
      x,
      lod,
      \(x, lod) round_lod.default(
        x,
        lod,
        threshold = threshold,
        na.replace = na.replace,
        accuracy = accuracy
      )
    )
    if (tibble::is_tibble(x))
      dplyr::bind_rows(res, .id = .id)
    else
      as.data.frame(dplyr::bind_rows(res, .id = .id))
    
  }


 
