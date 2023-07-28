# prepare_data2, print


#' prepare_data2
#'
#'  Funktion wird zum Aufbereiten der Daten verwendet. Die Daten werden als
#'  tibble::as_tibble() weitergegeben.
#'
#'
#'  Tabelle.default()
#'  errate_statistik3()
#'  APA.formula()
#'  corr_plot.formula()
#'  Hmisc_rcorr() also APA_Correlation
#'  
#' @param ... Formula, data usw
#'
#' @return Liste mit Namen und Daten
#' data,
#' measure.vars ,
#' group.vars,
#' condition.vars,
#' formula,
#' by,
#' measure,
#' row_name,
#' col_name,
#' measure.class,
#' group.class,
#' condition.class,
#' digits
#' @export
#' @examples
#'
#' dat<- data.frame(sex=1:2, m1=1:2,m2=1:2, m3=1:2, m4=1:2, m5=1:2, m6=1:2, geschl=1:2)
#'
#' prepare_data2(~ m1 + m2 + m3 + m4, dat)
#' prepare_data2(~ log(m1) + m2 + m3 + m4, dat)
#' prepare_data2(~ m1[1] + m2 + m3 + m4, dat)
#' prepare_data2(~ m1[1] + m2 + m3[4,median] + m4, dat)
#' prepare_data2(dat, m1, m2, m3, m4)
#' prepare_data2(dat, 4:7)
#' prepare_data2(dat, m1[1], m2, m3, m4)
#' prepare_data2(dat, m1[1], m2, m3[4,median], m4)
#' prepare_data2(dat, m1 , m2, m3=median, m4)
#' prepare_data2(dat, m1, m2, m3 , m4, by =  ~ geschl)
#' prepare_data2(dat, m1[4, median], m2, m3 , m4[5], by =  ~ geschl)
#'
#'

prepare_data2 <- function(...){
  UseMethod("prepare_data2")
}


#' @rdname prepare_data2
#'
#' @param x formel
#' @param data  data.frame
#' @param na.action na.pass, na.omit
#' @param groups condition
#' @param drop.unused.levels an factor
#'
#' @export
prepare_data2.formula <-
  function(x,
           data,
           groups = NULL,
           na.action = na.pass,
           drop.unused.levels = FALSE) {
    
    lbl <- get_label2(data)
    fm  <- cleaup_formula(x, data, groups)
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
      measure         = fm$measure,
      measure.test    = fm$measure.test,
      row_name        = lbl[fm$measure.vars],
      col_name        = lbl[fm$group.vars],
      measure.class   = fm$measure.class,
      group.class     = fm$group.class,
      condition.class = fm$condition.class,
      digits          = fm$digits,
      N               = nrow(dat)
    )
    
    class(stp25Data) <- c("stp25data", "list")
    stp25Data
  }


#' @rdname prepare_data2
#'
#' @param ... Namen oder Nummern (y-Variablen))
#' @param by  x-Variablen
#'
#' @export
prepare_data2.data.frame <- function(data,
                                     ...,
                                     by = "1",
                                     groups = NULL,
                                     na.action = na.pass,
                                     drop.unused.levels=FALSE) {
  hsub <- "h__"
  hend <- "__h"
  sub_haeding <- c()
  
  measure.vars <-
    sapply(lazyeval::lazy_dots(...), function(x) {
      if (!is.character(x$expr))
        as.character(x[1])
      else{
        sub_haeding <<- c(sub_haeding, as.character(x[1]))
        paste0(hsub , length(sub_haeding), hend)
      }
    })

  # abfangen vo prepare_data2(data, . ~ gender)
  if (grepl('~', measure.vars[1]))
    return(
      prepare_data2.formula(
        x =   as.formula(measure.vars[1]),
        data = data,
        groups = groups,
        na.action  = na.action,
        drop.unused.levels = drop.unused.levels
      )
    )
    

  # Leere Daten fuer die Zwischen-Ueberschrift
  if( !is.null(sub_haeding ) ){
    nn <- ncol(data)
    data[ paste0(hsub, seq_along(sub_haeding), hend) ] <- NA
    
    for (n in seq_along(sub_haeding))
      attr(data[[n+nn]], "label") <- sub_haeding [[n]]
  }
  
 
   # Fall prepare_data2(data)
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

  prepare_data2.formula(
    x = fm,
    data = data,
    na.action = na.action,
    drop.unused.levels = drop.unused.levels
  )
  
}



# Data --------------------------------------------------------------------




#' select_data
#' 
#' model.frame
#' Formula::Formula splitet log(m1) + m2 + m3 + m4 ~ g richtig auf
#' @noRd
select_data <-   function(formula,
                          data,
                          na.action = NULL,
                          drop.unused.levels = FALSE) {
  formula <-  Formula::Formula(formula)
  data <- if (is.null(na.action))
    stats::model.frame(formula, data,
                       drop.unused.levels = drop.unused.levels)
  else
    stats::model.frame(formula,
                       data,
                       na.action = na.action,
                       drop.unused.levels = drop.unused.levels)
  
  names(data) <- all.vars(formula)
  
  if (tibble::is_tibble(data))
    data
  else
    tibble::as_tibble(data)
}





#' default_measure
#' 
#' auswertungs Methode
#' @noRd
default_measure <- function(measure, measure.vars, measure.class) {
    # cat(" \n in default_measure\n  ")
  # print(measure)
  # print(measure.vars )
  # print(measure.class)
  if (length(measure) == 1) {
    measure <- measure.class
  }
  else{
    nas <- which(is.na(measure))
    measure[nas] <- measure.class[nas]
  }
  
  if( any(is.na( measure ))) 
    stop("Achtung eine Variable ist doppelt!\n",
         paste(names(measure), collapse=", "))
  
  names(measure) <- measure.vars
  measure 
}


#' digits
#' 
#' @noRd
default_digits <- function(digits, 
                           measure.vars, 
                          # measure.class,
                           measure
                           ) {
  
  if (length(digits) == 1) {
    digits <-  ifelse(measure == "factor", get_opt("prozent", "digits"),
               ifelse(measure == "multi", get_opt("prozent", "digits"), 
               ifelse(measure == "mean", get_opt("mean", "digits"),  
               ifelse(measure == "median", get_opt("median", "digits"),
               ifelse(measure == "numeric", get_opt("mean", "digits"), 0
               )))))
  }
  else{
    nas <- which(is.na(digits))
    digits[nas] <-
               ifelse(measure[nas] == "factor", get_opt("prozent", "digits"),
               ifelse(measure[nas] == "multi", get_opt("prozent", "digits"), 
               ifelse(measure[nas] == "mean", get_opt("mean", "digits"),  
               ifelse(measure[nas] == "median", get_opt("median", "digits"),
               ifelse(measure[nas] == "numeric", get_opt("mean", "digits"), 0
                                  )))))
  }
  names(digits) <- measure.vars
  digits
}







# Satatistics -------------------------------------------------------------



#' which_test
#' 
#' 
#' @examples 
#' which_test( "factor", NULL)
#' which_test( "factor", "logical")
#' which_test( "numeric", "factor")
#' 
#' which_test(c("median", "mean", "logical", "numeric", "multi"),
#' "factor",
#' c(NA, "ttest", NA, NA, NA))
#' 
#' c(
#'   median = "contest" ,
#'   mean = "ttest",
#'   logical = "cattest" ,
#'   numeric = "contest" ,
#'   multi = "notest"
#' )
#' @noRd
which_test <-
  function(measure,
           group.class=NULL,
           measure.test = NULL,
           # test = c("catTest", "conTest", "ordTest", "noTest", "corTest")
           catTest = c("factor", "freq", "logical", "multi", "ratio"),
           conTest = c("numeric", "integer", "mean", "median")
  ) {
    
    #  cat("\n m: ", measure,"\ng: ",group.class, "\nt: ", measure.test, "\n" )
    rslt <-  sapply(measure, function(measure) {
      if (is.null(group.class)) { "notest"
      }
      else if (group.class == "factor") {
        if (measure %in% catTest) "cattest"
        else if (measure %in% conTest) "contest"
        
        else
          "notest"
      } else if (group.class == "numeric") {
        if (measure %in%  conTest) "cortest"
        else  "notest"
      } else "notest"
    })
    
    if (!is.null(measure.test)) {
      i <-  which(!is.na(measure.test))
      rslt[i] <- measure.test[i]
    }
    rslt
  }



#
# contest <-
#   c("contest",
#     "wilcox",
#     "utest",
#     "htest",
#     "kruskal",
#     "ttest",
#     "aov",
#     "anova")
# cattest <-  c("cattest", "chisq", "fisher", "ordtest", "binomial")
# notest <-  c("notest")
# ordtest <-  c("ordtest")
# disttest <-  c("shapiro", "kstest")
# cortest <-   c("pearson", "kendall", "spearman")
# stattest <-   c(contest, cattest, notest, disttest)



#' default test methods
#' 
#' @param x string
#' @param search_string  such-String
#'
#' @noRd
stp25_test_methode <- function(x,
                               search_string = c(
                                 "contest", "wilcox", "utest" , "htest"  ,
                                 "kruskal", "ttest" , "aov",  "anova",
                                 "cattest","chisq",  "fisher", "ordtest",
                                 "binomial","notest" , "shapiro", "kstest"
                               )) {
  rslt <-
    stringr::str_extract(tolower(x), 
                         paste0(search_string, collapse = "|")
    )
  
  if (all(is.na(rslt))) NULL else rslt
}



#' is_empty
#'
#' @param x A object to be tested
#' @noRd
is_empty2 <- function (x) {
  if (length(x) == 0) TRUE
  else if (length(x) == 1) {
    if (is.null(x)) TRUE
    else if (is.na(x)) TRUE
    else if (x == "") TRUE
    else FALSE
  }
  else  FALSE
}


# helpers -----------------------------------------------------------------


#' Variablen als Nummer
#' 
#' @param measure.vars  variablen-String
#' @param data daten
#'
#' @noRd
cleaup_names <- function(measure.vars, data) {
  measure <- makeNamesNum(measure.vars, data)
  
  if (any(measure == "" | is.na(measure))) {
    measure <- measure[measure != ""]
    measure <- measure[!is.na(measure)]
  }
  
  measure
}



#' class  
#'
#' Arbeiten mit mehrfachen Classen.
#'
#' @param data data.frame
#'
#' @noRd
get_classes <-
  function(data) {
    sapply(data, function(x) 
      setdiff(class(x), c("labelled", "ordered")))
  }



#' @noRd
#' @description makeNamesNum: aus Nummern die Namen extrahieren
#' @param  data Daten als data.frame
#' @param  measure Variablen
#' @param  meAsNum  logical welche sind Zahlen
#' @return   string( )
#' @examples
#'
#' #  measure <- c("geschl", "1" , "3:5", 1)
#' #  stp25tools:::makeNamesNum(measure,  data=dat)
#'
makeNamesNum <- function(measure,
                         data,
                         meAsNum = grepl("^[[:digit:]]", measure)
){
  if (sum(meAsNum) == 0)
    return(measure)
  measure_number <- NULL
  for (i in seq_len(length(meAsNum))) {
    if (meAsNum[i]) {
      if (grepl("[^[:digit:]]", measure[i])) {
        n <- stringr::str_split(measure[i], ":", 2)
        
        measure_number <- c(measure_number,
                            names(data)[seq(n[[1]][1], n[[1]][2])])
      }
      else
        measure_number <-
          c(measure_number, names(data)[as.numeric(measure[i])])
    }
    else
      measure_number <- c(measure_number, measure[i])
  }
  unique(measure_number)
}




# Formula -----------------------------------------------------------------

#' @param x A object to be tested
#' @noRd
is_formula2 <- function (x) {
  inherits(x, "formula")
}



#' @param formula,data,groups  input
#' @noRd
cleaup_formula <- function(formula, 
                           data, 
                           groups) {
 
  measure <- digits<- NA
  if (!is.null(groups)) {
    # das ist nicht schoen aber es funktioniert auch bei langen Formeln
    warnings(" prepare_data2.formula : benutze Gruppen als condition.vars!")
    condition.vars <- gsub("~", "", deparse(groups))
    formula <-  paste(deparse(formula), collapse = "")
    formula <-  formula(paste(formula, "|", condition.vars))
  }
  
  formula <- clean_dots_formula(formula, names_data = names(data))
  
  frml <- formula_split(formula)
  formula <- frml$formula
  dedect_string_test <- NULL
  
  if (any(all.names(formula[[2L]]) %in% '[')) {
    #  bei var[2,median] kommt der Median durch, 
    #  error wegen  width.cutoff = 60L
    y_hsd <-
      gsub(" ", "", paste(deparse(formula[[2L]]), collapse = ""))
    y_hsd <- strsplit(y_hsd, "\\+")[[1]]
    # bereinigen von Klammern
    measure.vars <- gsub("\\[.+\\]", "", y_hsd) 
    measure <- as.character(rep(NA, length(measure.vars)))
    dedect_string_test <- measure
    digits <- as.integer(rep(NA, length(measure.vars)) )
    names(digits) <- measure.vars
    names(measure) <- measure.vars
    
    # Dedect Position
    pos <- grep('\\[', y_hsd)
    # dedect_string afer ,  var[2,median]  gsub("[^[:alpha:]]", "", "var[2,median]")
    dedect_string <- gsub("[^[:alpha:]]", "",
                          stringr::str_extract(y_hsd[pos], "\\[.+"))
    
    dedect_test <- stp25_test_methode(dedect_string)
    # return:"mean"   "freq"   "median" NA
    dedect_string <- stp25_stat_methode(dedect_string) 
    
    dedect_number <- as.integer(gsub("[^0-9]", "",
                                     stringr::str_extract(y_hsd[pos], "\\[.+")))
    
    
    if (!is.null(dedect_test)) {
      for (i in  seq_len(length(pos)))
        if (!is_empty2(dedect_test[i]))
          dedect_string_test[pos[i]] <- dedect_test[i]
    }
    
    if (!is_empty2(dedect_string)) {
      for (i in  seq_len(length(pos)))
        if (!is_empty2(dedect_string[i]))
          measure[pos[i]] <- dedect_string[i]
    }
    
    if (!is_empty2(dedect_number)) {
      for (i in seq_len(length(pos)))
        if (!is_empty2(dedect_number[i]))
          digits[pos[i]] <- dedect_number[i]
    }
    
    if (length(formula) == 2) {
      formula <- to_formula(measure.vars, NULL)
      
    } else {
      x_hsd <- strsplit(deparse(formula[[3L]]), " \\+ ")[[1]]
      group.vars <- gsub("\\[.+\\]", "", x_hsd)
      formula <- to_formula(measure.vars, group.vars)
    }
  }
  
  measure.vars <- all.vars(formula[[2L]]) 
  measure.class <- get_classes(data[measure.vars])
  
  if (any(is.na(measure)))
    measure <- default_measure(measure, measure.vars, measure.class)
  
   # clean measre 
  measure <- gsub("freq", "factor", measure)

  if (any(is.na(digits)))
    digits <- default_digits(digits, measure.vars, measure)
  
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
  
  #' Texte also Überschfifte werden zu logical mit NA
  #' daher hie die Heder vergeben
  if (any(measure == "logical")) {
    logik <-  which(measure == "logical")
    any_missing <-
      sapply(data[measure.vars[logik]], function(x)
        length(na.omit(x)))
    
    measure[logik] <-
      ifelse(
        measure[logik]  == "logical" & (any_missing == 0),
        "header", measure[logik])
  }
   
  list(
    formula         = formula,
    by              = by,
    measure.vars    = measure.vars,
    group.vars      = group.vars,
    condition.vars  = condition.vars,
    measure         = measure,
    measure.test    = which_test(measure, group.class[1], dedect_string_test),
    digits          = digits,
    measure.class   = measure.class,
    group.class     = group.class,
    condition.class = condition.class,
    all.vars        = if(is.null(condition.vars)) formula 
                     else update(formula, formula(
                       paste("~ . +", 
                             paste(condition.vars, collapse="+"))))
  )
}



#' @noRd
#' @description make_formula: Formel erstellen in \code{berechne_all(...)} verwendet. 
#' Hier wird \code{cbind(a,b,c)~g} ausgegebeb.
#' @param  measurevar,groupvars  mamen als strings
#' @examples
#'
#'
#' make_formula("a", "b")
#' make_formula("a", c("b","c"))
#' make_formula("a", ~b+c)
#' make_formula(c("a", "d"), c("b","c"))
#' 
make_formula <- function(measurevar,
                         groupvars=NULL) {
  if (is.null(groupvars))
    return(formula(paste("~", paste(
      measurevar, collapse = "+"
    ))))
  
  if (is_formula2(groupvars))
    groupvars <- paste0(all.vars(groupvars), collapse = "+")
  else
    groupvars <- paste0(groupvars, collapse = "+")
  
  if (is_formula2(groupvars)) {
    measurevar <- all.vars(measurevar)
    if (length(measurevar) != 1)
      measurevar <-
        paste("cbind(", paste0(measurevar, collapse = ", "), ")")
  }
  else {
    if (length(measurevar) != 1)
      measurevar <-
        paste("cbind(", paste0(measurevar, collapse = ", "), ")")
  }
  
  formula(paste(measurevar, "~", groupvars))
}


#' formel generieren
#'
#' in prepare_data2 
#'
#' @param measure.vars.group.vars,condition.vars mamen als strings
#'
#' @noRd
to_formula <-
  function(measure.vars,
           group.vars,
           condition.vars = NULL) {
    
    if (is.null(group.vars)) {
      fm <- paste0("~", paste(measure.vars, collapse = "+"))
    }
    else if (group.vars[1] == "1") {
      fm <- paste0("~", paste(measure.vars, collapse = "+"))
    }
    else {
      if (is_formula2(group.vars))
        fm <- paste0(paste(measure.vars, collapse = "+"),
                     "~",
                     paste(all.vars(group.vars), collapse = "+"))
      else
        fm <- paste0(paste(measure.vars, collapse = "+"),
                     "~",
                     paste(group.vars, collapse = "+"))
    }
    
    formula(fm)
  }



#' @noRd
#' @param x  Formula
#' @param data,names_data data.frame
#' 
#' @description clean_dots_formula: Formel bereinigen
#' @return clean_dots_formula: formula - Objekt
#' 
#' @examples
#'
#' data <- data.frame(x = NA, y = NA, z = NA)
#' stp25tools:::clean_dots_formula(x ~ y, data)
#' stp25tools:::clean_dots_formula(. ~ x + y, data)
#' stp25tools:::clean_dots_formula(x + y ~ ., data)
#' stp25tools:::clean_dots_formula(~., data)
#' stp25tools:::formula_split(a+b~x|y)
#'
clean_dots_formula <- function(x,
                               data = NULL,
                               names_data = names(data)) {
  
#  cat( "   in clean_dots_formula\n")
  myvars <- all.vars(x)
  
#  print( myvars )
  
  if (any(myvars %in% ".")) {
    
    if (length(myvars) == 1) {
      return(formula(paste(
        " ~ ", paste(names_data, collapse = "+")
      )))
    } 
    else if (myvars[1] == ".") {
      var_dots <- names_data[!names_data %in% myvars[-1]]
      return(formula(paste(
        paste(var_dots, collapse = "+"),
        " ~ ",
        paste(myvars[-1], collapse = "+")
      )))
    } 
    else if (myvars[length(myvars)] == ".") {
      var_dots <- names_data[!names_data %in% myvars[-length(myvars)]]
      return(formula(paste(
        paste(myvars[-length(myvars)],
              collapse = "+"),
        " ~ ",
        paste(var_dots, collapse = "+")
      )))
    }
    
  } 
  else {
    return(x)
  }
  
}




#' @noRd
#' @description formula_split stolen from mosaic ggformula
#' @return formula_split: liste  formula, condition, facet_type
#'
formula_split <- function(x) {
  # split A | B into formula <- A; condition <- B
  fs <-
    stringr::str_split(paste(deparse(x), collapse=""), "\\|")[[1]]
  # try to split, else leave formula unchanged and set condition to NULL
  if ((length(fs) != 2) ||
      !tryCatch({
        formula_string <- fs[1]
        condition_string <- fs[2]
        if (!grepl("~", condition_string)) {
          condition_string <- paste0("~", condition_string)
          condition <-
            as.formula(condition_string, env = environment(x))
          facet_type <- "facet_wrap"
        } else {
          condition <-
            as.formula(condition_string, env = environment(x))
          facet_type <- "facet_grid"
        }
        x <-
          as.formula(formula_string, env = environment(x))
        TRUE
      }
      , error = function(e) {
        warning(e)
        FALSE
      })) {
    condition <- NULL
    facet_type <- "none"
  }
  list(formula = x,
       condition = condition,
       facet_type = facet_type)
}




# Print -------------------------------------------------------------------


# Print Methode
#
#' @export
#' @noRd
print.stp25data <- function(x, ...) {
  cat("\nformula: ")
  print(x$formula)
  cat("\nmeasure.vars: ", paste(x$measure.vars, collapse = ", "))
  cat("\nmeasure: ", paste(x$measure , collapse = ", "))
  cat("\nmeasure.class: ", paste(x$measure.class , collapse = ", "))
  cat("\ndigits: ", paste(x$digits, collapse = ", "))
  cat("\nrow_name: ", paste(x$row_name, collapse = ", "))
  cat("\nby: ")
  print(x$by)
  cat("\ngroup.vars: ", paste(x$group.vars, collapse = ", "), "\n")
  #  cat("\ncol_name: ", paste(x$col_name, collapse=", "),"\n")
  print(head(x$data))
}

