#' Long und Wide  
#'
#' Erweiterung von tidyr::pivot_longer tidyr::pivot_wider
#'  
#' @param x data.frame oder formula
#' @param data data
#' @param key,value Namen fuer die Ausgabe
#' @param use.label attribut label verwenden
#' @param by,id.vars Items
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
#' df2<-df |> Wide(student, A, B)
#' 
#' 
#' 
#' df[-4] |> tidyr::spread(student, A)
#' df[-4] |> Wide(student, A)
#' 
#' df2
#' df2  |> Long( A_Amy, A_Bob ,B_Amy, B_Bob, by=~month)
#' 
#' 
#' 
#' 
#' 
#' df |>
#'   tidyr::gather(variable, value, -(month:student)) |>
#'   tidyr::unite(temp, student, variable) |>
#'   tidyr::spread(temp, value)
#'   
#'  # Wide 
#'  
#' dat <- data.frame(
#' month = rep(1:3, 2),
#' student = factor(rep(c("Amy", "Bob"), each = 3)),
#' A = c(19, 27, 16, 28, 10, 29),
#' B = c(6, 7, 8, 5, 6, 7)
#' )
#' #dat |> Wide(student,  A, B)
#' # dat |> Wide( month ~ student, value="A")
#' dat |> Wide(student)
#' dat |> Wide(student,  B)
#' 
#' dat |> Wide(A ~ student)
#' 
#' dat |> Wide(A + B ~ student)
#' dat |> Wide(A + B ~ student + month)
#' 
#' 
#' dat |> Wide(student,  A, B)
#' 
#' 
#' dat |> Wide(A ~ student)
#' dat |> Wide(A ~ student + month)
#' 
#' 
#' dat |> Wide(month ~ student, A)
#' dat |> Wide(month ~ student, A, B)
#' 
Long <- function(x, ...) {
  UseMethod("Long")
}



#' @rdname Long
#' @export
Long.formula <- function(x,
                         data,
                         key = "variable",
                         value = "value",
                         use.label=TRUE,
                         ...) {
  x <- clean_dots_formula(x, names_data = names(data))
  rhs <- all.vars(x[-3])
  lhs <- all.vars(x[-2])
  
  data <- data[c(rhs, lhs)]
  

  
  if (use.label)
    lvl <-  get_label2(data[rhs])
  else {
    lvl <- rhs
    names(lvl) <- rhs
  }
  
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
Long.data.frame <- function(x,
                            ...,
                            by = NULL,
                            key = "variable",
                            value = "value",
                            id.vars = all.vars(by),
                            use.label=TRUE) {
  measure.vars <-
    sapply(lazyeval::lazy_dots(...), function(y) {
      as.character(y[1])
    })
  
  if(length(measure.vars)==0){ 
    measure.vars  <- 
      if(length(id.vars)==0) names(x)  else names(x[-id.vars])
    }
  else {
    if (length(measure.vars) == 1 & grepl('~', measure.vars[1])) {
      return(Long.formula(formula(measure.vars[1]), x,  key, value))
    }
    else {
      measure.vars <- cleaup_names(measure.vars, x)
    }
       
    x <- x[c(measure.vars, id.vars)]
    
    }
  
  if( length(unique(measure.vars)) != length(measure.vars)) 
    stop(" In Long.data.frame sind die Variablen-Namen (measure.vars) doppelt!")
  if (use.label)
    lvl <- get_label2(x[measure.vars])
  else {
    lvl <- measure.vars
    names(lvl) <- measure.vars
  }
  rstl <-
    tidyr::pivot_longer(x, 
                        cols = measure.vars,
                        names_to = key, values_to =value)
  
  rstl[[key]] <- factor(rstl[[key]], names(lvl), lvl)
  
  rstl
}
