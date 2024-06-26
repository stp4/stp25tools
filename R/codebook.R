#' Codebook
#' 
#' @description  codebook: Create a structure of the data as data.frame.
#'
#' @param x  Data.frame
#'
#' @return data.frame
#' @export
#'
#' @examples
#'  \dontrun{
#' 
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
#' cdb <- codebook(DF)
#' 
#' #' Haendisch aendern
#' cdb$Value.Labels[2] <- "factor: Big | Small"
#' 
#' 
#' }
codebook <- function(x) {
  lbl <- get_label(x)
  value_lbl <- sapply(x, function(y) {
    if (is.factor(y))
      paste("factor:", paste(levels(y), collapse = " | "))
    else
      paste(class(y)[1], ":", sep="")
  })
  
  data.frame(names = names(lbl),
             label = lbl,
             value.labels = value_lbl)
  
  
}


#' @rdname codebook
#' @param file File (xlsx)
#' @export
#' 
#' @examples
#'  
#' #' Speichern des Codebook
#'  save_codebook(
#'     DF,
#'     "codebook.xlsx"
#'  )
#'  
#'  
save_codebook <- function(x, 
                          file = "codebook.xlsx")  {
  cat("\n", paste(getwd(),  file,  sep = "/"), "\n")
  cdb<- codebook(x)
  writexl::write_xlsx(list(codebook=cdb),
                      path = file)
  invisible(cdb)
}


#' @rdname codebook
#' @description  use_codebook: Restore the structure of the data from the 
#' Excel file created by the Codebook function.
#'
#' @param data data.frame, default is NULL
#' @param codebook data.frame codebook = codebook(DF), default is NULL
#' @param sheet.data,sheet.codebook  file= "demo.xlsx", sheet.data = 1, sheet.codebook = 2,
#' @param names,label,value.labels Variablen NNamen im codebook
#'
#' @return data.frame or tibble
#' @export
#'
#' @examples
#'  \dontrun{
#'  
#' #' Labels und Factoren ubernehmen  
#'    use_codebook(
#'       DF, 
#'       file = "demo.xlsx", 
#'       sheet.codebook = 2
#'       )
#' 
#' #' Nur die Labels  ubernehmen  
#'    use_codebook(
#'          DF, 
#'          file = "demo.xlsx", 
#'          sheet.codebook = 2,
#'          value.labels = NULL
#' ) 
#' 
#' #' Strucktur aus einem anderen data.frame uebernehmen
#' #  use_codebook(DF1, codebook(DF))
#' 
#' #' Daten und Strucktur im Excell-File
#' #  use_codebook(file = "demo.xlsx")
#' }
use_codebook <-
  function(data = NULL,
           codebook = NULL,
           file = "demo.xlsx",
           sheet.data = 1,
           sheet.codebook = 2,
           names = "names",
           label = "label",
           value.labels = "value.labels") {
    if (is.null(data)) {
      cat("\nUse data from file", file, "\n")
      data <-
        readxl::read_excel(file,
                           sheet = sheet.data)
      print(head(data))
    }else{
      cat("\nI am using the provided data.\n")
    }
    
    if (is.null(codebook)) {
      cat("\nLabel and levels from file", file, "\n")
      codebook <-
        readxl::read_excel(file,
                           sheet = sheet.codebook)
      print(head(codebook))
    }
    
    label <- codebook[[label]]
    names(label) <- codebook[[names]]
    
    
    
    if (!is.null(value.labels)) {
      
      for (i in grep("factor\\: ", codebook[[value.labels]])) {
        
        fct <- codebook[[value.labels]][i]
        fct <- gsub("factor\\: ", "", fct)
        fct <- unlist(stringr::str_split(fct, " \\| "))
        
        old <- data[[codebook[[names]][i]]]
        if (is.character(old))  {
          cat("\n", codebook[[names]][i] , ": character -> factor")
          
          
          data[[codebook[[names]][i]]] <- factor(old, fct)
        }
        else if (is.factor(old)) {
          
         if( identical(levels(old), fct) ) {
           cat("\n", codebook[[names]][i] , ": no change")
         }
          else{
          cat("\n", codebook[[names]][i] , ": factor -> factor")
            
            if(nlevels(old) != length(fct) ){
              
              cat("\n\nFehler!!\n\n Old: ", nlevels(old), "New: ", length(fct),"\n")
              cat("\n Old: \n")
              
              print(levels(old))
              cat("\n New: \n")
              
              print(fct)
              cat("\n")
            }
            
          data[[codebook[[names]][i]]] <- 
            factor(as.numeric(old), seq_len(nlevels(old)), fct)
          }
          }
        else if (is.numeric(old)) {
          cat("\n", codebook[[names]][i] , ": numeric -> factor")
          data[[codebook[[names]][i]]] <- factor(old,
                                                 seq_len(nlevels(old)),
                                                 fct)
        }
      }
      
      for (i in grep("numeric\\:", codebook[[value.labels]])) {
        if (!is.numeric(data[[codebook[[names]][i]]])) {
          cat("\n", codebook[[names]][i] , ": numeric -> factor")
          data[[codebook[[names]][i]]] <-
            as.numeric(as.character(data[[codebook[[names]][i]]]))
        }
        cat("\n", codebook[[names]][i] , ": no change")
      }
      
    }
    set_label2(data, label)
  }




#' @rdname codebook
#' @description  save_data: Write an Excel or SPSS file.
#'
#' @return Invisibly returns the combined data frame that is written to
#'         the csv-file.
#'

#' @param x data.frame
#'
#' @param file file to write to, or just file name (to write to working directory).
#' @param sep The field separator string. In some Western European locales, Excel
#'            uses a semicolon by default, while in other locales the field
#'            separator string in Excel is a comma. 
#' @param row.names,include.codebook logical include something
#' @importFrom utils write.csv2 write.csv
#' @importFrom  writexl write_xlsx
#' @export
#' @examples 
#' 
#' \dontrun{
#' # require(stp25tools)
#' dat <-  Label(
#'   data.frame(
#'   month = rep(1:3, 2),
#'   student = rep(c("Amy", "Bob"), each = 3),
#'   A = c(19, 27, 16, 28, 10, 29),
#'   B = c(6.45, 7.47, 8.76, 5.01, 6.91, 3.47)
#'   ),
#'   month="Monat", student="Student", A= "Anzahl", B= "B-Score" )
#' 
#' 
#' #' Speichern der Kompletten Strucktur
#' save_data(dat, "demo.xlsx")
#' }
#' 
save_data <- function(x,
                      file,
                      sep = ",",
                      row.names = FALSE,
                    #  include.labels=FALSE,
                    # gelöschte wegen codebook
                    
                      include.codebook=TRUE) {
  if(include.codebook)
    cdb <- codebook(x)
  
 
  # check if file extension exists
  has.extension <- (regexpr("\\.[^\\.]*$", file) != -1)
  if (!has.extension)
    file <- paste0(file, ".csv")
  
  # check fir valid file extention
  dot.start <- regexpr("\\.[^\\.]*$", file) + 1
  ext <- tolower(substring(file, dot.start, nchar(file)))
  
  
  # tell user what's going on...
  cat("Writing file to:\n")
  cat(normalizePath(
    path = file,
    winslash = "/",
    mustWork = FALSE
  ))
  cat("\n\n")
  if (ext == "csv") {
    if(include.codebook ) cat("\n Codebook geht nur mit .xlsx!\n")
    
    # write to excel
    if (sep == ";")
      utils::write.csv2(x, file = file, fileEncoding = "UTF-8")
    else
      utils::write.csv(x, file = file, fileEncoding = "UTF-8")
  }
  else if (ext == "xlsx" | ext == "xls") {
    if(include.codebook )
      writexl::write_xlsx(list(data= x, codebook=cdb),  path = file)
    else   
    writexl::write_xlsx(x, path = file)
    
  }
  else{
    stop("No valid file extention.")
  }
  
  
  
  # return data frame
  invisible(x)
}


#' @rdname codebook
#'
#' @param ... Text
#' @param file file to write to default INFO.txt
#'
#' @return file name
#' @export
#'
#' @examples
#' \dontrun{
#'  save_text(
#' "Analyse der Auswirkungen des Homeoffice während der
#' Corona-Pandemie, insbesondere auf 
#' Schlafverhalten, Physis und Psyche“ Titel der Arbeit"
#' )
#' }
save_text <- function(..., file = "INFO.txt") {
  msg <- paste(..., sep = "")
  # msg <- gsub("#' ", "", msg)
  msg <-  paste(Sys.Date(), "\n", msg, sep = "")
  if (!file.exists(file)) {
    cat(msg, file = file)
  } else{
    cat("\n\n",
        msg,
        file = file,
        sep = "",
        append = TRUE)
  }
  file
}


