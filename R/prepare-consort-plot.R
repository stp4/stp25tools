#' prepare_consort
#'
#' @param ...  data.frame mit filter2
#' @param split table-Objekt
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' require(stp25stat2)
#' require(stp25tools)
#' 
#' 
#' data(DFdummy, package = "stp25data")
#' if(exists("DFdummy")){
#'   DF1 <- DFdummy |> filter2(study.agreement)
#'   attr(DF1, "filter")
#'   
#'   DF2 <- DF1 |> filter2(
#'     st.p.sars.cov2 == "nein",
#'     !is.na(spike.igg.3.impfung),
#'     !is.na(MPN)
#'     
#'   )
#'   
#'   DF3 <- DF2 |> filter2(
#'     study.agreement,
#'     sero.negativ.after.dose.2,
#'     !is.na(spike.igg.3.impfung),
#'     !is.na(spike.igg.4.impfung),
#'     spike.igg.3.impfung == "<7.1 BAU/ml"
#'   )
#'   
#'   
#'   
#'   dat <- prepare_consort(DF1, DF2, DF3)
#'   
#'   
#'   require(consort)
#'   
#'   out <- consort_plot(
#'     data = dat,
#'     orders = c(
#'       Trial.Nr   = "Population",
#'       Condition.1           = "Excluded",
#'       Trial.Nr     = "Allocated \nDeskriptive Analyse",
#'       Condition.2    =    "Fehlende Daten",
#'       Trial.Nr = "Regressionsanalyse",
#'       Condition.3    = "Not evaluable for the final analysis",
#'       Trial.Nr = "Final Analysis"
#'     ),
#'     side_box = c("Condition.1", "Condition.2", "Condition.3"),
#'     cex = 0.9
#'   )
#'   
#'   
#'   
#'   plot(out)
#' }
#' 
#' 
#' # DF1 <- DFdummy |> filter2(study.agreement)
#' # attr(DF1, "filter")
#' # DF2 <- DF1 |> filter2(
#' #   st.p.sars.cov2 == "nein",
#' #   !is.na(spike.igg.3.impfung),
#' #   !is.na(MPN)
#' # 
#' # )
#' # 
#' # DF3 <- DF2 |> filter2(
#' #   study.agreement,
#' #   sero.negativ.after.dose.2,
#' #   !is.na(spike.igg.3.impfung),
#' #   !is.na(spike.igg.4.impfung),
#' #   spike.igg.3.impfung == "<7.1 BAU/ml"
#' # )
#' # dat <- prepare_consort(DF1, DF2, DF3)
#' # 
#' # require(consort_plot)
#' # 
#' # out <- consort_plot(
#' #   data = dat,
#' #   orders = c(
#' #     Trial.Nr   = "Population",
#' #     Condition.1           = "Excluded",
#' #     Trial.Nr     = "Allocated \nDeskriptive Analyse",
#' #     Condition.2    =    "Fehlende Daten",
#' #     Trial.Nr = "Regressionsanalyse",
#' #     Condition.3    = "Not evaluable for the final analysis",
#' #     Trial.Nr = "Final Analysis"
#' #   ),
#' #   side_box = c("Condition.1", "Condition.2", "Condition.3"),
#' #   cex = 0.9
#' # )
#' # 
#' # plot(out)
#' # 
#' # 
#' # dat2 <- prepare_consort(DF1,
#' #                         DF2,
#' #                         split = table(DF2$geschlecht)
#' # )
#' # 
#' # consort_plot(
#' #   data = dat2,
#' #   orders = c(
#' #     Trial.Nr   = "Population",
#' #     Condition.1           = "Excluded",
#' #     Trial.Nr     = "Allocated \nDeskriptive Analyse",
#' #     Condition.2    =    "Fehlende Daten",
#' #     # Trial.Nr = "Regressionsanalyse",
#' #     Split = "Stratifizierung",
#' #     Trial.Nr = "Final Analysis"
#' #   ),
#' #   side_box = c("Condition.1", "Condition.2"),
#' # 
#' #   allocation = "Split",
#' #   labels = c("1" = "Screening", "2" = "Randomization", "4" = "Final"),
#' #   cex = 0.9
#' # )
#' 
#' }
#' 
#' 
prepare_consort <- function(..., split=NULL) {
  dots <- list(...)
  rslt <- NULL
  
  for (i in seq_along(dots)) {
    x <- attr(dots[[i]], "filter")
    n_total <- x$Remaining.N[1]
    x_ex <-
      tidyr::uncount(dplyr::select(x,
                                   Criteria,
                                   Condition, 
                                   Excluded.N),
                     Excluded.N)
    x_ex$Criteria <-  paste(i,  x_ex$Criteria, sep=".")
    x_ex$Condition <- cleanup_symbols(x_ex$Condition)
    
    if (i == 1) {
      names(x_ex)[2] <- "Condition.1"
      x_in <- data.frame(
        Criteria = rep(NA, n_total - nrow(x_ex)),
        Condition.1 = rep(NA, n_total - nrow(x_ex))
      )
      x_ex <- rbind(x_ex, x_in)
      x_ex$Trial.Nr <- seq_len(nrow(x_ex))
      rslt <- x_ex[c(3, 1, 2)]
    }
    else{
      next_var <- paste0("Condition.", i)
      rslt[next_var] <- NA
      pos <-   which(is.na(rslt$Criteria))[seq_len(nrow(x_ex))]
      rslt$Criteria[pos] <- x_ex$Criteria
      
      rslt[[next_var]][pos] <- x_ex$Condition
    }
    
  }
  if(!is.null(split)) {
    if(!is.table(split)) stop("Split muss table sein!")
    rslt$Split<- NA  
    pos <- which(is.na(rslt$Criteria))
    rslt$Split[pos] <- unlist(mapply(rep, names(split), (split),  USE.NAMES = FALSE))
    
    
  }
  
  
  rslt
}

cleanup_symbols <- function(x) {
  x <- gsub("==", "=", x)
  x <- gsub("!is.na", "missing", x)
  x
}