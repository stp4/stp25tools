#' filter2
#' 
#' Keep rows that match a condition,  Subsetting Vectors, Matrices and Data Frames
#' 
#' @description filter2: stolen from dplyr::filter  and https://github.com/openpharma/visR
#'
#' @param .data 	A data frame,
#' @param ... Expressions
#' @param .preserve Relevant when the .data input is grouped.
#' @param subject_column_name Unique subject id
#'
#' @return data.frame mit attr(x, "filter")
#' @export
#' @examples
#' 
#'  subset2(airquality, Temp > 80, select = c(Ozone, Temp))
#'  dplyr::filter(airquality, Temp > 80)
#'  
#'  dat <- filter2(airquality, Temp > 80 )
#'  
#'  # simple_consort_plot(dat)
#'  attr(dat, "filter")
#'
filter2 <- function(.data,
                    ...,
                    #  .by = NULL,
                    .preserve = FALSE,
                    subject_column_name = NULL) {
  data <- dplyr::filter(.data, ..., .preserve = .preserve)
  
  filter_expressions <-
    sapply(lazyeval::lazy_dots(...), function(x) {
      as.character(x[1])
    })
  
  if (is.null(subject_column_name)) {
    # cat( "\n Erwarte Daten im Wide-Format und generiere selbst eine Id!\n")
    subject_column_name <- "filter__id"
    .data$filter__id <- seq_len(nrow(.data))
  }
  
  cohort_table <-
    visR_get_attrition(.data,
                       criteria_conditions = filter_expressions,
                       subject_column_name = subject_column_name)
  
  attr(data, "filter") <- cohort_table
  data
}


#' @rdname filter2
#' @description subset2: subset + label_data_frame
#'
#' @param x object to be subsetted.
#' @param ... 	further arguments to be passed to or from other methods.
#'
#' @return data.frame
#' @export
#'
subset2 <- function(x,
                    ...){
  label_data_frame(subset(x, ...), get_label(x))
}


#' @rdname filter2
#' 
#' @description  prepare_consort erstellt der Tabelle auc mit filter2 excludet data
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
prepare_consort <- function(..., 
                            split=NULL) {
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



#' @noRd
#' 
#' @description  visR_get_attrition:
#' Generate cohort attrition table
#' 
#' Geändert sind die Variablen-Namen
#'
#'
#' @param data Dataframe. It is used as the input data to count the subjects
#' that meets the criteria of interest
#' @param criteria_descriptions \code{character} It contains the descriptions
#' of the inclusion/exclusion criteria.
#' Each element of the vector corresponds to the description of each criterion.
#' @param criteria_conditions \code{character} It contains the corresponding
#' conditions of the criteria.
#' These conditions will be used in the table to compute the counts of the
#' subjects.
#' @param subject_column_name \code{character} The column name of the table that
#' contains the subject id.
#'
#' @return The counts and percentages of the remaining and excluded subjects
#' for each step of the cohort selection in a table format.
visR_get_attrition <- function(data,
                               criteria_descriptions =
                                 paste0(seq_along(criteria_conditions), ". Filter"),
                               criteria_conditions,
                               subject_column_name) {
  
  
  # if (!inherits(subject_column_name, "character") || length(subject_column_name) > 1) {
  #   stop("The 'subject_column_name' argument has to be a string. Please correct the 'subject_column_name' and re-run the function")
  # }
  #
  # if (!subject_column_name %in% names(data)) {
  #   stop("The 'subject_column_name' argument doesn't correspond to a column name. Please correct the 'subject_column_name' and re-run the function")
  # }
  
  # if (length(criteria_descriptions) != length(criteria_conditions)) {
  #   stop("Vectors 'criteria_descriptions' and 'criteria_conditions' must have the same length.")
  # }
  
  if (!NA %in% criteria_conditions) {
    criteria_map <-
      data.frame(
        cbind(criteria_descriptions, criteria_conditions),
        stringsAsFactors = FALSE)
    
    final_cond <- c()
    person_count_master <- c()
    
    for (each_cond in criteria_map$criteria_conditions) {
      final_cond <- ifelse(is.null(final_cond),
                           each_cond,
                           paste(paste0("(", final_cond, ")"),
                                 paste0(paste0("(", each_cond), ")"),sep = " & ")
      )
      # print(final_cond)
      person_count_temp <-
        data |>
        dplyr::filter(eval(parse(text = final_cond))) |>
        dplyr::select(!!subject_column_name) |>
        dplyr::n_distinct()
      # print(person_count_temp)
      
      person_count_master <-
        c(person_count_master, person_count_temp)
    }
    
    if (length(person_count_master) > 0) {
      count_master_table <-
        tibble::tibble("Remaining.N" = person_count_master)
      criterion_0 <- 
        tibble::tibble(
          criteria_conditions = "none",
          criteria_descriptions = "Total cohort size",
          Remaining.N = dplyr::select(data,!!subject_column_name) |>
            dplyr::n_distinct()
        )
      
      # generate attrition table
      attrition_table <-
        criterion_0 |>
        dplyr::bind_rows(cbind(criteria_map, count_master_table)) |>
        dplyr::mutate(
          Remaining.prc = round(100 * Remaining.N / max(Remaining.N), 1),
          Excluded.N = dplyr::lag(Remaining.N,n = 1L,default = max(Remaining.N)) - Remaining.N,
          Excluded.prc = round(100 * Excluded.N / max(Remaining.N), 1)
        ) |>
        # rename columns
        dplyr::rename(Condition = criteria_conditions,
                      Criteria = criteria_descriptions) |>
        # fix formatting
        dplyr::select(Criteria, Condition, dplyr::everything())
      
      class(attrition_table) <- c("attrition", class(attrition_table))
      return(attrition_table)
    }
  }
}
