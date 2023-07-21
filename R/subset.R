#' Subsetting Vectors, Matrices and Data Frames
#'
#' @param x object to be subsetted.
#' @param ... 	further arguments to be passed to or from other methods.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' 
#' subset2(airquality, Temp > 80, select = c(Ozone, Temp))
#' 
subset2 <- function(x,
                    ...){
  label_data_frame(subset(x, ...), get_label(x))
}