#' Keep rows that match a condition
#'
#'copy from filter {dplyr} and https://github.com/openpharma/visR
#'
#' @param .data 	A data frame,
#' @param ... Expressions
#' @param .preserve Relevant when the .data input is grouped.
#' @param subject_column_name Unique subject id
#'
#' @return data.frame mit attr(x, "filter")
#' @export
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








#' Generate cohort attrition table
#'
#'
#' Gestolen von  https://github.com/openpharma/visR
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



#' consort_plot
#'
#' @param x data.frame
#' @param ... an  stp25plot:::visr
#'
#' @return  plot
#' @export
#'
simple_consort_plot <- function(x, ...) {
  stp25plot:::visr(attr(x, "filter"),  ...)
}
