% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string-clean-names.R
\name{fix_names}
\alias{fix_names}
\title{Fix names of data.frames}
\usage{
fix_names(
  x,
  tolower = TRUE,
  abbreviate = TRUE,
  name_length = 25,
  replace = c(`\%` = "_pct", `#` = "_cnt", `¨` = "", `&+` = "_and_", `@+` = "_at_", `_`
    = ".")
)
}
\arguments{
\item{x}{Data.frame}

\item{tolower}{lower case}

\item{abbreviate, name_length}{Abbreviate strings to at least name_length characters,}

\item{replace}{character to replace}
}
\value{
Data.frame
}
\description{
Insbesondere bei `readxl::read_excel` werden die Spaltennamen
genau so übernommen, wie sie in Excel vorkommen, also mit allen Sonderzeichen.
Diese Funktion packt die Namen in die Labels und bereinigt die Spaltennamen.
Die Funktion ist praktisch die gleiche wie `clean_names()`, aber `fix_names()` 
ist einfacher aufgebaut. 
`clean_names()` enthält die Funktion `cleanup.encoding()`.
#'
}
\examples{

#   fl_new <- "Raw data/Mai_Markiert_LT_Cases_Jan2018-Dez2022.xlsx"
# DF <- readxl::read_excel(fl_new, range = "A1:z15", sheet = 1) |> 
#   fix_names()
}
