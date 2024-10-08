% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_to.R
\name{add_to}
\alias{add_to}
\alias{add_to.list}
\alias{add_to.data.frame}
\title{add_to}
\usage{
add_to(x, ...)

\method{add_to}{list}(x, ...)

\method{add_to}{data.frame}(x, ...)
}
\arguments{
\item{x}{Dataframe or list}

\item{...}{what to add}
}
\value{
data.frame or list
}
\description{
add_to
}
\examples{

#' Eine oder mehere Zeilen zu einem data.fram hinzufügen

df <-   data.frame(
  Source = c("A", "B", "C", "F"),
  x = 1:4,
  y = 1:4,
  stringsAsFactors = FALSE
)

df
add_to(df, "Erste Zeile" = 1, "Dritte" = 3)
add_to(df, "Erste Zeile" = 1, "letzte" = -1)
add_to(df, list("G", 5), pos = -1)
add_to(df, data.frame(Source = c("G", "H"), x = 5:6), pos = -1)


# add something to list
 
my_setting <- list(
  a = list(alpha = 1, col = "2"),
  b = list(alpha = 1, col = "2"),
  c = list(alpha = 1, col = "2")
)
my_setting <- add_to(my_setting,
                    c = list(fill = "gray80"),
                    b = list(alpha = 2, beta = 3))
my_setting$c

 # tibble::add_column(.data )
}
