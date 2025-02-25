#' @rdname prepare_data2
#' 
#' @description
#' prepare_data(): wie prepare_data2() aber ohne zusatz infos in den [] Klammern.
#' es werden keine measure und digits ausgegeben.
#' Die measure.vars sind das was links steht und die group.vars das was rechts steht.
#' 
#' 
#' @export
#' @examples
#'  
#' dat<- data.frame(sex=factor(1:2), 
#'                  m1=1:2,m2=1:2, m3=1:2, m4=1:2, m5=1:2, m6=1:2, age=1:2)
#' 
#' dat |>  prepare_data(m1 + m2 + m3 ~ sex)
#' dat |>  prepare_data(m1 + m2 + m3 ~ sex | age)
#'
prepare_data <- function(...){
  UseMethod("prepare_data")
}


#' @rdname prepare_data2
#'
#'
#' @export
prepare_data.formula <-
  function(x,
           data,
           groups = NULL,
           na.action = na.pass,
           drop.unused.levels = FALSE,
           ...) {
    
    lbl <- get_label2(data)
    fm  <- cleaup_formula2(x, data, groups)
    dat <- select_data(fm$all.vars,
                       data,
                       na.action,
                       drop.unused.levels)
    
    stp25Data <- list(
      data            = dat,
      measure.vars    = fm$measure.vars,
      group.vars      = fm$group.vars,
      condition.vars  = fm$condition.vars,
      formula         = fm$formula,
      by              = fm$by,
      measure         = NULL,
      measure.test    = NULL,
      row_name        = lbl[fm$measure.vars],
      col_name        = lbl[fm$group.vars],
      measure.class   = fm$measure.class,
      group.class     = fm$group.class,
      condition.class = fm$condition.class,
      digits          = NULL,
      N               = nrow(dat),
      formula_in      = x
    )
    
    class(stp25Data) <- c("stp25data", "list")
    stp25Data
  }


#' @rdname prepare_data2
#'
#'
#' @export
prepare_data.data.frame <- function(data,
                                     ...,
                                     by = "1",
                                     groups = NULL,
                                     na.action = na.pass,
                                     drop.unused.levels = FALSE) {

  
  measure.vars <-
    sapply(lazyeval::lazy_dots(...), function(x) {
      if (!is.character(x$expr))
        as.character(x[1])
    })
  
  # abfangen von prepare_data(data, . ~ gender)
  if (grepl('~', measure.vars[1]))
    return(
      prepare_data.formula(
        x =   as.formula(measure.vars[1]),
        data = data,
        groups = groups,
        na.action  = na.action,
        drop.unused.levels = drop.unused.levels
      )
    )
  
  # Fall prepare_data(data)
  if(length(measure.vars) == 0) measure.vars <- names(data)
  else measure.vars <- cleaup_names(measure.vars, data)
  
  # Fehlercheck
  if (length(setdiff(measure.vars, names(data))) > 0) {
    missing_measure.vars <- setdiff(measure.vars, names(data))
    nn <- ncol(data)
    data[missing_measure.vars] <- NA
    
    for (n in seq_along(missing_measure.vars))
      attr(data[[n + nn]], "label") <- paste("Error:", missing_measure.vars[n], "dose not exist!")
  }
  
  fm <-
    to_formula(
      measure.vars = measure.vars,
      group.vars = by,
      condition.vars = groups
    )
  
  prepare_data.formula(
    x = fm,
    data = data,
    na.action = na.action,
    drop.unused.levels = drop.unused.levels
  )
  
}



#' @noRd
cleaup_formula2 <- function(formula, 
                           data, 
                           groups) {
  
  if (!is.null(groups)) {
    # das ist nicht schoen aber es funktioniert auch bei langen Formeln
    warnings(" prepare_data.formula : benutze Gruppen als condition.vars!")
    condition.vars <- gsub("~", "", deparse(groups))
    formula <-  paste(deparse(formula), collapse = "")
    formula <-  formula(paste(formula, "|", condition.vars))
  }
  
  formula_in <-
    formula <-
    clean_dots_formula(formula, names_data = names(data))
  #  a + b + c ~ 1
  if (length(formula) == 3L)
    if (formula[[3L]] == 1) formula[[3L]] <- NULL
  
  frml <- formula_split(formula)
  formula <- frml$formula
  
  if (any(all.names(formula[[2L]]) %in% '[')) {
    warning("Zusatz infos in [] werden ignoriert!")
    y_hsd <-
      gsub(" ", "", paste(deparse(formula[[2L]]), collapse = ""))
    y_hsd <- strsplit(y_hsd, "\\+")[[1]]
    # bereinigen von Klammern
    measure.vars <- gsub("\\[.+\\]", "", y_hsd) 
    
    if (length(formula) == 2) {
      formula <- to_formula(measure.vars, NULL)
    } 
    else {
      x_hsd <- strsplit(deparse(formula[[3L]]), " \\+ ")[[1]]
      group.vars <- gsub("\\[.+\\]", "", x_hsd)
      formula <- to_formula(measure.vars, group.vars)
    }
  }
  
  measure.vars <- all.vars(formula[[2L]])
  
  if (length(setdiff(measure.vars,  names(data))) > 0) {
    cat("\n Das wurde Uebergeben: ")
    print(measure.vars)
    cat("\n\n und diese sind falsch: \n")
    print(setdiff(measure.vars,  names(data)))
    stop("Die oben ausgegebenen Variablen sind nicht in den Daten vorhanden")
  }
  
  measure.class <- get_classes(data[measure.vars])
  
  in_vars <- strsplit(as.character(formula[[2L]])[2L], " \\+ ")[[1L]]
  dupl_measure <- duplicated(in_vars)
  if(any(dupl_measure)) {
    warning("stp25tools::prepare_data():\n Es wurden folgende Parameter mehrfach uebergeben:\n" ,
            paste( in_vars[dupl_measure], collapse =", "),
            "\n  Sollte das gewollt sein bitte bei \nTbll_desc(..., use.duplicated = TRUE) \nentsprechend die Einstellungen vornehmen."
    )
  }
  

  if (length(formula) == 3L ){
    group.vars <-  all.vars(formula[[3L]])
    by <- formula(paste("~", paste(group.vars, collapse="+")))
    group.class <- get_classes(data[group.vars])
  }
  else{
    group.vars<-  group.class<- NULL
    by<- "1"
  }
  
  if(!is.null(frml$condition)){ 
    condition.vars <- all.vars(frml$condition)
    condition.class <- get_classes(data[condition.vars]) 
  }
  else{
    condition.vars<-condition.class <- NULL
  }
  
 
  list(
    formula         = formula,
    formula_in      = formula_in,
    by              = by,
    measure.vars    = measure.vars,
    group.vars      = group.vars,
    condition.vars  = condition.vars,
    measure.class   = measure.class,
    group.class     = group.class,
    condition.class = condition.class,
    all.vars        = if(is.null(condition.vars)) formula 
    else update(formula, formula(
      paste("~ . +", 
            paste(condition.vars, collapse="+"))))
  )
}



 


