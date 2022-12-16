#' Codebook
#'
#' @param x  Data.frame
#'
#' @return
#' @export
#'
#' @examples
#' #' require(stp25tools)
#' df <- data.frame(
#'   BMI = c(1, 2, 3, 1, 2, 3),
#'   WHtR = gl(2, 3, label = c("Amy", "Bob")),
#'   WHtR_1 = c(9, 7, 6, 8, 6, 9),
#'   bildprof = c(6, 7, 8, 5, 6, 7)
#' )
#' 
#' DF <-
#'   Label(df,
#'         BMI = "Body-Mass-Index",
#'         WHtR =  "Waist-Height-Ratio",
#'         WHtR_1 = "Waist-Height-Ratio")
#' 
#' 
#' 
#' cdb<- codebook(DF)
#' save_codebook(DF)
#' cdb
#' 
#' cdb$Value.Labels[2] <- "factor: Big | Small"
#' 
#' (use_codebook(df, cdb))
codebook <- function(x) {
  lbl <- get_label(x)
  value_lbl <- sapply(x, function(y) {
    if (is.factor(y))
      paste("factor:", paste(levels(y), collapse = " | "))
    else
      paste(class(y), collapse = ",")
  })
  
  data.frame(names = names(lbl),
             label = lbl,
             value.labels = value_lbl)
  
  
}


#' @rdname codebook
#' @param file File (xlsx)
#' @export

save_codebook <- function(x, 
                          file = "codebook.xlsx")  {
  cat("\n", paste(getwd(),  file,  sep = "/"), "\n")
  writexl::write_xlsx(codebook(x), path = file)
}


use_codebook <-
  function(data,
           codebook,
           names = "names",
           label = "label",
           value.labels = "value.labels") {
    label <- codebook[[label]]
    names(label) <- codebook[[names]]

    if( !is.null(value.labels)){
    for (i in grep("factor\\: ", codebook[[value.labels]])) {
      data[[codebook[[names]][i]]]
      fct <- codebook[[value.labels]][i]
      fct <- gsub("factor\\: ", "", fct)
      fct <- unlist(stringr::str_split(fct, " \\| "))
      old <- data[[codebook[[names]][i]]]
      
      data[[codebook[[names]][i]]]   <-  factor(as.numeric(old),
                                                seq_len(nlevels(old)),
                                                fct)
      
    }}
    
    
    stp25tools:::set_label2(data, label)
    
  }


