#' Long und Wide  
#'
#'  Erweiterung von tidyr::pivot_longer tidyr::pivot_wider
#' @param x data.frame oder formula
#' @param ... weitere Argument 
#'
#' @return data.frame
#' @export
#' @examples 
#' 
#' df <- data.frame(month=rep(1:3,2),
#' student=rep(c("Amy", "Bob"), each=3),
#' A=c(9, 7, 6, 8, 6, 9),
#' B=c(6, 7, 8, 5, 6, 7))
#' 
#' df2<-df %>% Wide(student, c(A, B))
#' 
#' 
#' 
#' df[-4] %>% tidyr::spread(student, A)
#' df[-4] %>% Wide(student, A)
#' 
#' 
#' df2  %>% Long( Amy_A, Amy_B, Bob_A, Bob_B, by=~month)
#' 
#' 
#' 
#' 
#' df %>%
#'   tidyr::gather(variable, value, -(month:student)) %>%
#'   tidyr::unite(temp, student, variable) %>%
#'   tidyr::spread(temp, value)
Long <- function(x, ...) {
  UseMethod("Long")
}

#' @rdname Long
#' @export
#' 
#' @examples 
#' 
#'   Long( .~ month, df2)
#'   
Long.formula <- function(x,
                         data,
                         key = "variable",
                         value = "value",
                         ...) {
  x <- clean_dots_formula(x, names_data = names(data))
  rhs <- all.vars(x[-3])
  lhs <- all.vars(x[-2])
  
  data <- data[c(rhs, lhs)]
  lvl <-  get_label2(data[rhs])
  # all.vars elimieniert doppelte namen
  # if( length(unique(rhs)) != length(rhs)) 
  #   stop(" In Long.formula sind die Variablen-Namen doppelt!")
  
  rstl <-
    tidyr::pivot_longer(data,
                        cols = rhs,
                        names_to = key,
                        values_to = value)
  
  rstl[[key]] <- factor(rstl[[key]], names(lvl), lvl)
  
  rstl
}

#' @rdname Long
#' @export
Long.data.frame <- function(data,
                            ...,
                            by = NULL,
                            key = "variable",
                            value = "value",
                            id.vars = all.vars(by)) {
  measure.vars <-
    sapply(lazyeval::lazy_dots(...), function(x) {
      as.character(x[1])
    })
  
  if(length(measure.vars)==0){ 
    measure.vars  <- 
      if(length(id.vars)==0) names(data)  else names(data[-id.vars])
    }
  else {
    if(length(measure.vars)==1 & grepl('~', measure.vars[1] ))
      return( Long.formula(formula(measure.vars[1]), data,  key, value) )
       
    data <- data[c(measure.vars, id.vars)]
    
    }
  
  if( length(unique(measure.vars)) != length(measure.vars)) 
    stop(" In Long.data.frame sind die Variablen-Namen (measure.vars) doppelt!")
  
  lvl <-  get_label2(data[measure.vars])
  rstl <-
    tidyr::pivot_longer(data, 
                        cols = measure.vars,
                        names_to = key, values_to =value)
  
  rstl[[key]] <- factor(rstl[[key]], names(lvl), lvl)
  
  rstl
}
