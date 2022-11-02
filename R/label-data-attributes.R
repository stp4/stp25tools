# Label, delet_label, get_label, set_label, 








#' Label
#'
#' Setzen des attr(data, "label")
#' @param data data.frame
#' @param ... label in der Form a="Hallo, b="Welt"
#' @export
#' @examples
#'
#' df <- data.frame(
#' BMI=c(1,2,3,1,2,3),
#' WHtR= gl(2,3, label =c("Amy", "Bob")),
#' WHtR_1=c(9,7,6,8,6,9),
#' bildprof=c(6,7,8,5,6,7)
#' )
#'
#' DF<-
#'   Label(df, BMI = "Body-Mass-Index",
#'         WHtR =  "Waist-Height-Ratio",
#'         WHtR_1 ="Waist-Height-Ratio"
#'   )
#'
#' # DF$BMI<- units::set_units(DF$BMI, kg/m2)
#'
#' get_label(DF)
#' get_label(DF, include.units=TRUE)
#' DF<- set_label(DF, c(bildprof = "Bildungsprofil"))
#' get_label(DF)
#' DF<- delet_label(DF)
#' get_label(DF)
#'
#' DF<- wrap_label(DF)
#' get_label(DF)
#'
Label <- function(data, ...) {
  lbl <- list(...)
  if (length(lbl) == 0) {
    message("Label: Keine label gefunden!")
    return(data)
  } else{
    if (is.null(names(lbl))) {
      message("Label: Keine Namen gefunden! Verwende daher names(data)")
      names(lbl) <-  names(data)[1:length(lbl)]
    }
    set_label(data, unlist(lbl))
  }
}


#' @rdname Label
#' @description delet_label:  Loeschen aller Attributs label
#'
#' @export
delet_label <- function(data) {
  for (n in names(data))
    attr(data[[n]], "label") <- NULL
  data
}



#' @rdname Label
#' @param include.units einheiten
#' @param label named character string
#' @description set_label:  Setzen  der Attributs label
#' @param label label als Character-String mit Namen c(a="Hallo, b="Welt")
#'
#' @export
set_label <- function(data,
                      label = NULL,
                      include.units = FALSE) {
  if (is.null(label)) {
    message("Warnung set_label: Keine label gefunden!\n")
    return(data)
  } else {
    return(set_label2(data, label))
  }
  
}

#' @rdname Label
#' @param pattern Pattern to look for.
#'
#' @export
#'
trimm_label<- 
  function(data, pattern = "\\.\\.\\. ") {
    lbl <- get_label2(data)
    lbl_trm <-  stringr::str_split_fixed(lbl, pattern, n = 2)
    #print(lbl_trm[,2] )
    lbl_trm <- ifelse(lbl_trm[,2] == "",  lbl_trm[,1], lbl_trm[,2])
    names(lbl_trm) <- names(lbl)
    set_label2(data, lbl_trm)
  }

#' @rdname Label
#' @param pattern Pattern to look for.
#'
#' @export
#'
gsub_label <- function(data, pattern = "\\&amp;", replacement="&") {
  lbl <- get_label2(data)
  
  lbl_trm <-  gsub(pattern, replacement, lbl ) 
  names(lbl_trm) <- names(lbl)
  
  set_label2(data, lbl_trm)
}


#' @rdname Label
#' @description get_label und GetLabelOrName:  Abrufen  der Attributs label
#' @export
#'
get_label <-
  function(data, ...,
           include.units = FALSE) {
    measure.vars <-
      sapply(lazyeval::lazy_dots(...),
             function(x) {as.character(x[1])})
    
    if (length(measure.vars) > 0) data <- data[measure.vars]
    
    lbl <- lapply(data, attr, "label") 
    if (length(lbl) == 0)
      return(NULL)
    
    unlabl <- which(sapply(lbl, is.null))
    
    lbl[unlabl] <- names(lbl[unlabl])
    lbl <- unlist(lbl)
    #  }
    
    if (include.units) {
      is_units <- sapply(data, function(z)
        inherits(z, "units"))
      if (any(is_units)) {
        lbl_nams <- names(lbl)
        
        lbl_units <-
          sapply(data, function(z)
            if (inherits(z, "units"))
              paste0(" [", as.character(attr(z, "units")), "]")
            else
              "")
        lbl <-  paste0(lbl, lbl_units)
        names(lbl) <- lbl_nams
      }
    }
    lbl
  }


# get_label <- function(data,
#                       include.units = FALSE) {
#   lbl <- lapply(data, attr, "label")
#   if (length(lbl) == 0)
#     return(NULL)
#   
#   unlabl <- which(sapply(lbl, is.null))
#   
#   lbl[unlabl] <- names(lbl[unlabl])
#   lbl <- unlist(lbl)
#   #  }
#   
#   if (include.units) {
#     is_units <- sapply(data, function(z)
#       inherits(z, "units"))
#     if (any(is_units)) {
#       lbl_nams <- names(lbl)
#       
#       lbl_units <-
#         sapply(data, function(z)
#           if (inherits(z, "units"))
#             paste0(" [", as.character(attr(z, "units")), "]")
#           else
#             "")
#       lbl <-  paste0(lbl, lbl_units)
#       names(lbl) <- lbl_nams
#     }
#   }
#   lbl
# }




#' set_label2
#' 
#' internal use
#' 
#' @param data data.frame
#' @param label attribut label
#' @noRd
set_label2 <- function(data, label = NULL) {
  nms <- names(data)
  nl <- nms %in% names(label)
  if (sum(nl) > 0) {
    for (n in nms[nl])
      attr(data[[n]], "label") <- label[[n]]
  }
  data
}


#' get_label2 
#' 
#' internal use
#'
#' @noRd
#'
#' @param data data.frame
#' 
get_label2 <- function(data) {
  lbl <- lapply(data, attr, "label")
  if (length(lbl) == 0)
    return(NULL)
  
  unlabl <- which(sapply(lbl, is.null))
  
  lbl[unlabl] <- names(lbl[unlabl])
  unlist(lbl)
  
}



# @rdname Label
# @param include.units Einheiten
# @export
#
#GetLabelOrName <- function(data, include.units = TRUE) {
#get_label(data, include.units)
#}

#' @rdname Label
#' @param data data.frame
#'
#' @param label label als named vector
#'
#' @description  Intern wenn mit get_label nur die Kopie wiederhergestellt wird.
#'
label_data_frame  <- function(data,
                              label) {
  if (all(names(data) %in% names(label))) {
    for (i in names(data)) {
      attr(data[[i]], "label") <- label[[i]]
    }
    data
  }
  else
    set_label(data, label)
}
