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
#'
#'  lbl <- c (
#' 'Heart Failure' = 1,
#' 'Rhythm Abnormality' = 2,
#' 'Valve Dysfunction' = 3,
#' 'Bleeding with OAC' = 4,
#' 'ACS' = 5,
#' 'Neurological Event' = 6,
#' 'Neoplastic Disease' = 7,
#' 'Others' = 8,
#' 'No Complications' = 0
#' )
#'
#' x <- c(0,
#'        "1,2,3,6,8",
#'        2,
#'        "1,2,8",
#'        3.8,
#'        "2,4,6,8")
#'
#' x <- gsub("\\.", ",", x)
#'
#' rslt <- separate_multiple_choice(x ,
#'                                  sep = ",",
#'                                  as_logical = TRUE,
#'                                  label = lbl)
#' stp25stat2::Tbll_desc(rslt)
#'
#'
#'
#'
#' dat <-  data.frame(Q1=c(134,NA,35,134,5,24),
#'                    Q2=c(
#'
#'                      "Alm Dudler, Essig, Cola",
#'                      NA,
#'                      "Cola, Holer",
#'                      "Alm Dudler, Cola, Essig",
#'                      "Holer",
#'                      "Bier, Essig"))
#' #dat
#'
#'
#' dat<- cbind(dat,separate_multiple_choice(dat$Q2))
#'
#' dat<- cbind(dat,separate_multiple_choice(dat$Q1,
#'                                          label=c(
#'                                            "Alm Dudler","Bier", "Cola", "Essig", "Holer") ))
#' names(dat)<- GetLabelOrName(dat)
#' dat
#' x<-data.frame(A=c(15911,261011,3711,4811))
#' separate_multiple_choice(x$A)
separate_multiple_choice <- function(x,
                                     sep = ", ",
                                     #   levels,labels,
                                     
                                     out_levels = 1:0,
                                     out_labels = c("ja", "nein"),
                                     as_logical = FALSE,
                                     prafix =  NULL,
                                     into = NULL,
                                     label = NULL,
                                     na.strings = NULL) {
  # callingFun = as.list(sys.call(-1))[[1]]
  # calledFun = as.list(sys.call())[[1]]
  if (!is.null(na.strings))
    x[which(x == na.strings)] <- NA
  
  if (is.null(prafix))
    prafix <-
      paste0(gsub("^.*\\$", "", deparse(substitute(x))), "_")
  
  
  is_numeric_x <- FALSE
  
  if (is.numeric(x)) {
    is_numeric_x <- TRUE
    x <- as.character(x)
    x <- gsub("10", "a", x)
    x <- gsub("11", "b", x)
    
    x <- gsub("(.)\\B", "\\1,", x)
    sep <- ","
  }
  
  
  prafix <- gsub("[^[:alnum:]_]", "", prafix)
  #leere levels nicht  entfernen sondern behalten
  x <- as.character(x)
  x[is.na(x)] <- "XX_9999"
  x[x==""] <- "XX_9999"
  # if(missing(levels))
  x <- factor(x)
  
  # else    x <- factor(x, levels)
  cat("\n----------------------------------------------------------------\n")
  cat("Warnung: wenn komische Leerzeichen daher kommen gut aufpassen!\nDas was unten kommt wird aufgedröselt.\n")
   print(levels(x))
   cat("\n----------------------------------------------------------------\n")
  # separate braucht die Anzahl an Levels
  unique_elements <-
    unique(unlist(stringr::str_split(x, sep)), use.names = FALSE)
  
  #print(unique_elements)
  if (is.null(into))
    into <- paste0("M", 1:length(unique_elements))
  
  # id ist zur kontrolle und name xxxx damit es zu keinen konflikten kommt
  res <-  data.frame(id = 1:length(x), xxxx = x)
  
  res <-
    tidyr::separate(res,
                    "xxxx",
                    into = into,
                    sep = sep,
                    remove = F)  ## Aufsplitten
  res <-
    na.exclude(tidyr::gather(res, q2 , val, -id, -xxxx))   ## breit zu lang
  
  res <- tidyr::spread(dplyr::mutate(res, q2 = 1), val, q2)
  
  
  res[-1:-2][is.na(res[-1:-2])] <- 0
  
  if (any(names(res) == "XX_9999")) {
    res[which(res$XX_9999 == 1),-1] <- NA
    res <- res[,-ncol(res)]
  }
  
  
  
  lbl <- names(res)[-1:-2]
  # Check if string contains ONLY NUMBERS
  if (!grepl("\\D", paste(lbl, collapse = "")))
    names(lbl) <- paste0(prafix, lbl)
  else
    names(lbl) <- paste0(prafix, 1:length(lbl))
  
  
  names(res)[-1:-2] <- names(lbl)
  if (is.null(out_labels))
    res <- res[-1:-2]
  else
    res <- dapply2(res[-1:-2],
                   function(z) {
                     if (as_logical)
                       z == out_levels[1]
                     else
                       factor(z, out_levels, out_labels)
                     
                   })
  
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
    
  } else{
    if (is.null(label)) {
      res <-  set_label(res, lbl)
    }
    else if (!is.null(names(label))) {
      lbl <- names(label)
      names(lbl) <- paste0(prafix, as.vector(label))
      res <-  set_label(res, lbl)
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
#  lbl <- c (
# 'Heart Failure' = 1,
# 'Rhythm Abnormality' = 2,
# 'Valve Dysfunction' = 3,
# 'Bleeding with OAC' = 4,
# 'ACS' = 5,
# 'Neurological Event' = 6,
# 'Neoplastic Disease' = 7,
# 'Others' = 8,
# 'No Complications' = 0
# )
# 
# x <- c(0,
#        "1,2,3,6,8",
#        2, "",
#        "1,2,8",
#        3.8,
#        "2,4,6,8")
# 
# x <- gsub("\\.", ",", x)
# 
# rslt <- separate_multiple_choice(x ,
#                                  sep = ",",
#                                  as_logical = TRUE,
#                                  label = lbl)
# stp25stat2::Tbll_desc(rslt)