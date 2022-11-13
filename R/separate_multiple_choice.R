#' separate multiple choice
#'
#' Aufdroeseln von Mehrfachantworten
#'
#' \code{separate_multiple_choice()} Aufdröseln vom Mehrfachantworten, die Funktion transformiert einen String mit Trennzeichen
#' zu einem Multi-Set mit 0 und 1. (Separate multiple choice)
#' @param x Vektor ist entweder ein Character oder eine zahl
#' Bei Zahlen gehen nur 1 bis 11
#' @param sep  wichtig ist das trennzeichen
#' @param prafix neue Variablen Name fuer die ausgabe default ist der Name vom Input
#' @param as_logical ausgabe logical
#' @param out_labels,out_levels Outut nimmeric oder factor
#' @param into  brauch nicht gesetzt werden entspricht der Anzahl an levels
#' @param label Labels
#' @return data.frame mit 0 1 Kodierung und den Fragen als Labels
#' @export
#'
#' @examples
#'
#'  #require(stp25tools)
#' lbl <- c (
#'   '1 Heart Failure' = 1,
#'   '2 Rhythm Abnormality' = 2,
#'   '3 Valve Dysfunction' = 3,
#'   '4 Bleeding with OAC' = 4,
#'   '5 ACS' = 5,
#'   '6 Neurological Event' = 6,
#'   '7 Neoplastic Disease' = 7,
#'   '8 Others' = 8,
#'   '0 No Complications' = 0
#' )
#' 
#' x <- c(0,
#'        "1,3,6,8,4",
#'        2,
#'        "",
#'        "8,4",
#'        #  3.8,100,
#'        "4,6,8,3",
#'        "2,3,4,5")
#' 
#' #x <- gsub("\\.", ",", x)
#' rslt <-
#'   separate_multiple_choice(x ,
#'                            sep = ",",
#'                            as_logical = TRUE,
#'                            label = lbl)
#' stp25stat2::Tbll_desc(rslt)
#' 
#' 
#' 
#' dat <-  data.frame(Q1=c(134, NA, 35, 134, 5, 24),
#'                    Q2=c(
#'                      "Alm Dudler, Essig, Cola",
#'                      NA,
#'                      "Cola, Holer",
#'                      "Alm Dudler, Cola, Essig",
#'                      "Holer",
#'                      "Bier, Essig"))
#' 
#' 
#' 
#' dat2 <- cbind(dat[2],
#'               separate_multiple_choice(dat$Q2))
#' names(dat2) <- get_label(dat2)
#' dat2
#' 
#' 
#' dat1 <- cbind(dat[1],
#'               separate_multiple_choice(
#'                 dat$Q1,
#'                 label = 
#'                   c("Alm Dudler", 
#'                     "Bier", 
#'                     "Cola", 
#'                     "Essig", 
#'                     "Holer")))
#' names(dat1) <- get_label(dat1)
#' dat1
#' 
#' 
#' x<-data.frame(A=c(15911, 261011, 3711, 48111))
#' separate_multiple_choice(x$A)
separate_multiple_choice <- function(x,
                                      sep = ", ",
                                      id = 1:length(x),
                                      
                                      out_levels = 1:0,
                                      out_labels = NULL ,
                                      #  c("ja", "nein"),
                                      as_logical =  is.null(out_labels),
                                      prafix =  NULL,
                                      into = NULL,
                                      label = NULL,
                                      na.strings = NULL) {
  # callingFun = as.list(sys.call(-1))[[1]]
  # calledFun = as.list(sys.call())[[1]]
  mssn <- "zz_9999"
  is_numeric_x <- FALSE
  
  if (!is.null(na.strings))   x[which(x == na.strings)] <- NA
  
  if (is.null(prafix))  prafix <- paste0(gsub("^.*\\$", "", deparse(substitute(x))), "_")
  prafix <- gsub("[^[:alnum:]_]", "", prafix)
  
  if (is.numeric(x)) {
    warning("Wenn als Codierung Zahlen verwendet werden geht das nur mit maximal 11 Attributen!")
    is_numeric_x <- TRUE
    x <- as.character(x)
    x <- gsub("10", "a", x)
    x <- gsub("11", "b", x)
    x <- gsub("(.)\\B", "\\1,", x)
    sep <- ","
  }
  else{
    x <- as.character(x)
  }
  
  
  #leere levels nicht  entfernen sondern behalten
  x[is.na(x)] <- mssn
  x[x == ""] <- mssn
  x <- factor(x)
  
  
  
  cat("\n----------------------------------------------------------------\n")
  cat("Warnung: wenn komische Leerzeichen daher kommen gut aufpassen!
Das was unten kommt wird aufgedröselt.\n"
  )
  print(levels(x))
  cat("\n----------------------------------------------------------------\n")
  
  
  # separate braucht die Anzahl an Levels
  unique_elements <-
    unique(unlist(stringr::str_split(x, sep)), use.names = FALSE)
  
  if (is.null(into))
    into <- paste("M", 1:length(unique_elements), sep = "")
  
  # id ist zur kontrolle und name xxxx damit es zu keinen konflikten kommt
  res <- data.frame(id = id, xxxx = x)
  ## Aufsplitten
  res <-
    tidyr::separate(res,
                    "xxxx",
                    into = into,
                    sep = sep,
                    remove = FALSE)  
  ## breit zu lang
  res <-
    na.exclude(tidyr::gather(res, q2 , val, -id, -xxxx))   
  res <- 
    tidyr::spread(dplyr::mutate(res, q2 = 1), val, q2)
  res[-1:-2][is.na(res[-1:-2])] <- 0
  
  
  if (any(names(res) == mssn)) {
    res[which(res[[mssn]] == 1), -1] <- NA
    res <- res[, -ncol(res)]
  }
  
  lbl <- names(res)[-1:-2]
  # Check if string contains ONLY NUMBERS
  if (!grepl("\\D", paste(lbl, collapse = "")))
    names(lbl) <- paste0(prafix, lbl)
  else
    names(lbl) <- paste0(prafix, 1:length(lbl))
  
  names(res)[-1:-2] <- names(lbl)
  
  # Return-Format
  if (is.null(out_labels)) {
    res <- res[-1:-2]
    if (as_logical)
      res <- dapply2(res, as.logical)
  }
  else{
    res <-
      dapply2(res[-1:-2], function(z)
        factor(z, out_levels, out_labels))
  }
  
  
  # Ver-labeln
  if (!is.null(label) & is_numeric_x) {
    
    lbl <- gsub("a", "10", lbl)
    lbl <- gsub("b", "11", lbl)
    
    x <- as.numeric(lbl)
    y <- 1:length(label)
    used <- intersect(x, y)
    not_used <- setdiff(y, used)
    names(res) <- paste0(prafix, used)
    for (i in   not_used) {
      res[paste0(prafix, i)] <- NA
    }
    names(label) <- paste0(prafix, 1:length(label))
    cf <- names(res)
    res[order(nchar(cf), cf)]
    res <-   set_label(res, label)
  }
  else{
    ## eigene labels
    if (is.null(label)) {
      res <-  set_label(res, lbl)
    }
    else if (!is.null(names(label))) {
      label  <-  factor(lbl, label, names(label))
      lbl2 <- as.character(label)
      names(lbl2) <- names(label)
      
      if (any(is.na(lbl2))) {
        strange <- which(is.na(lbl2))
        cat("\n----------------------------------------------------------------\n")
        cat("Warnung: hier stimmt was nicht!\nNicht alle labels sind eindeutig!\n")
        print( list(lbl=lbl,
                    Label =lbl2,
                    names = names(res),
                    'Fehlend'= lbl[strange] ))
        cat("\n----------------------------------------------------------------\n")
        
        stop(
          "Achtung:\n  Nicht alle labels sind eindeutig!\n  Folgendes Label kommt nicht vor: ",
          paste(lbl[strange] , collapse = ", ")
        )
        
        lbl2[strange] <- lbl[strange]
      }
      
      any_missing <- setdiff (names(lbl2) , names(res))
      
      if (length(any_missing) > 0) {
        for (i in any_missing)
          res[i] <- NA
      }
      
      res <- set_label(res, lbl2)
    }
    else {
      nms < -names(lbl)
      lbl <- label
      names(lbl) <- nms
      res <-  set_label(res, lbl)
    }
    
  }
  
  res
}



# require(stp25tools)
# lbl <- c (
#   '1 Heart Failure' = 1,
#   '2 Rhythm Abnormality' = 2,
#   '3 Valve Dysfunction' = 3,
#   '4 Bleeding with OAC' = 4,
#   '5 ACS' = 5,
#   '6 Neurological Event' = 6,
#   '7 Neoplastic Disease' = 7,
#   '8 Others' = 8,
#   '0 No Complications' = 0
# )
# 
# x <- c(0,
#        "1,3,6,8,4",
#        2,
#        "",
#        "8,4",
#        3.8,100,
#        "4,6,8,3",
#        "2,3,4,5")
# length(x)
# #x <- gsub("\\.", ",", x)
# 
# rslt <- separate_multiple_choice(x ,
#                                   sep = ",",
#                                   as_logical = TRUE,
#                                   label = lbl)
# stp25stat2::Tbll_desc(rslt)
# 
# 
# 
# separate_multiple_choice(c(1,2,3,4,10))





# separate_multiple_choice_old <- function(x,
#                                      sep = ", ",
#                                      #   levels,labels,
#                                      
#                                      out_levels = 1:0,
#                                      out_labels = c("ja", "nein"),
#                                      as_logical = FALSE,
#                                      prafix =  NULL,
#                                      into = NULL,
#                                      label = NULL,
#                                      na.strings = NULL) {
#   # callingFun = as.list(sys.call(-1))[[1]]
#   # calledFun = as.list(sys.call())[[1]]
#   if (!is.null(na.strings))
#     x[which(x == na.strings)] <- NA
#   
#   if (is.null(prafix))
#     prafix <-
#       paste0(gsub("^.*\\$", "", deparse(substitute(x))), "_")
#   
#   
#   is_numeric_x <- FALSE
#   
#   if (is.numeric(x)) {
#     is_numeric_x <- TRUE
#     x <- as.character(x)
#     x <- gsub("10", "a", x)
#     x <- gsub("11", "b", x)
#     
#     x <- gsub("(.)\\B", "\\1,", x)
#     sep <- ","
#   }
#   
#   
#   prafix <- gsub("[^[:alnum:]_]", "", prafix)
#   #leere levels nicht  entfernen sondern behalten
#   x <- as.character(x)
#   x[is.na(x)] <- "XX_9999"
#   x[x==""] <- "XX_9999"
#   # if(missing(levels))
#   x <- factor(x)
#   
#   # else    x <- factor(x, levels)
#   cat("\n----------------------------------------------------------------\n")
#   cat("Warnung: wenn komische Leerzeichen daher kommen gut aufpassen!\nDas was unten kommt wird aufgedröselt.\n")
#    print(levels(x))
#    cat("\n----------------------------------------------------------------\n")
#   # separate braucht die Anzahl an Levels
#   unique_elements <-
#     unique(unlist(stringr::str_split(x, sep)), use.names = FALSE)
#   
#   #print(unique_elements)
#   if (is.null(into))
#     into <- paste0("M", 1:length(unique_elements))
#   
#   # id ist zur kontrolle und name xxxx damit es zu keinen konflikten kommt
#   res <-  data.frame(id = 1:length(x), xxxx = x)
#   
#   res <-
#     tidyr::separate(res,
#                     "xxxx",
#                     into = into,
#                     sep = sep,
#                     remove = F)  ## Aufsplitten
#   res <-
#     na.exclude(tidyr::gather(res, q2 , val, -id, -xxxx))   ## breit zu lang
#   
#   res <- tidyr::spread(dplyr::mutate(res, q2 = 1), val, q2)
#   
#   
#   res[-1:-2][is.na(res[-1:-2])] <- 0
#   
#   if (any(names(res) == "XX_9999")) {
#     res[which(res$XX_9999 == 1),-1] <- NA
#     res <- res[,-ncol(res)]
#   }
#   
#   
#   
#   lbl <- names(res)[-1:-2]
#   # Check if string contains ONLY NUMBERS
#   if (!grepl("\\D", paste(lbl, collapse = "")))
#     names(lbl) <- paste0(prafix, lbl)
#   else
#     names(lbl) <- paste0(prafix, 1:length(lbl))
#   
#   
#   names(res)[-1:-2] <- names(lbl)
#   if (is.null(out_labels))
#     res <- res[-1:-2]
#   else
#     res <- dapply2(res[-1:-2],
#                    function(z) {
#                      if (as_logical)
#                        z == out_levels[1]
#                      else
#                        factor(z, out_levels, out_labels)
#                      
#                    })
#   
#   if (!is.null(label) & is_numeric_x) {
#     lbl <- gsub("a", "10", lbl)
#     lbl <- gsub("b", "11", lbl)
#     
#     x <- as.numeric(lbl)
#     y <- 1:length(label)
#     used <- intersect(x, y)
#     not_used <- setdiff(y, used)
#     
#     names(res) <- paste0(prafix, used)
#     for (i in   not_used) {
#       res[paste0(prafix, i)] <- NA
#     }
#     
#     
#     names(label) <- paste0(prafix, 1:length(label))
#     
#     cf <- names(res)
#     res[order(nchar(cf), cf)]
#     
#     res <-   set_label(res, label)
#     
#   } else{
#     if (is.null(label)) {
#       res <-  set_label(res, lbl)
#     }
#     else if (!is.null(names(label))) {
#       lbl <- names(label)
#       names(lbl) <- paste0(prafix, as.vector(label))
#       res <-  set_label(res, lbl)
#     }
#     else {
#       nms < -names(lbl)
#       lbl <- label
#       names(lbl) <- nms
#       res <-  set_label(res, lbl)
#     }
#     
#   }
#   
#   res
# }
# 
# 
#  
# 
# 
# 
# 
#  