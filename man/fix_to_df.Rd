% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fix_to_data_frame.R
\name{fix_to_df}
\alias{fix_to_df}
\alias{capture_print}
\alias{fix_to_df.default}
\alias{fix_to_tibble}
\alias{fix_to_df.efflist}
\alias{fix_to_df.list}
\alias{fix_to_df.data.frame}
\alias{fix_to_df.matrix}
\alias{fix_to_df.vector}
\alias{fix_to_df.ftable}
\alias{fix_to_df.table}
\title{Transformiere zu data.frame}
\usage{
fix_to_df(x, ...)

capture_print(x, ...)

\method{fix_to_df}{default}(x, ...)

fix_to_tibble(x, ...)

\method{fix_to_df}{efflist}(...)

\method{fix_to_df}{list}(x, ...)

\method{fix_to_df}{data.frame}(x, include.rownames = FALSE, ...)

\method{fix_to_df}{matrix}(x, include.rownames = TRUE, include.dimnames = FALSE, ...)

\method{fix_to_df}{vector}(x, ...)

\method{fix_to_df}{ftable}(x, dim_x = dimension(x), atrb = attributes(x), ...)

\method{fix_to_df}{table}(x, dim_x = dimension(x), ...)
}
\arguments{
\item{x}{Objekt}

\item{...}{an print}

\item{include.rownames, include.dimnames}{columns as first rownames}

\item{dim_x, atrb}{nicht zum aendern}
}
\value{
ein data.frame Objekt

data.frame
}
\description{
fix_levels ist fuer dta |> gather(Parameter, Wert, x1:x5)  |> fix_levels(dta)
fix_colnames ist fuer Formatieren in der Funktion \code{Output()}

capture_print:  Try to catch Output via print. Stolen from depigner::tidy_summary.R
}
\examples{

x <- list(
M1 = data.frame(
  Source = c("Intercept", "A", "B" , "C", "Residual"),
  b = c(0, 1, 2, 3, 0),
  y = c((1:4) + 10, 0)
),

M2 = data.frame(
  Source = c("Intercept", "A", "C", "B",  "D",  "E", "Residual"),
  x = c(0, 1, 3, 2, 4, 5, 0),
  y = c((1:6) + 21, 0)
),
M3 = data.frame(
  Source = c("A", "B", "C", "D", "Residual"),
  x = c((1:4), 0),
  y = c((1:4) + 20, 0)
),

M1 = data.frame(
  Source = c("A", "B",  "D", "Residual"),
  x = c(1, 2, 4, 0),
  y = c((1:3) + 30, 0)
)

)
dat <-   list(
  hoch = factor(c("A", "A", "A", "C", "C", "A"),   c("A", "B", "C", "D")),
  mittel = factor(c("B", "B", "B", "A", "B", "D"),   c("A", "B", "C", "D")),
  gering = factor(c("C", "C", "C", "B", "A", "B"),   c("A", "B", "C", "D"))
)


# liste mit data.frames
fix_to_df(x)

fix_to_df(dat)

fix_to_df(as.matrix(1:10))


mdat <- matrix(
  c(1, 2, 3, 11, 12, 13),
  nrow = 2,
  ncol = 3,
  byrow = TRUE,
  dimnames = list(c("row1", "row2"),
                  c("C.1", "C.2", "C.3"))
)
fix_to_df(mdat)
fix_to_df(table(rpois(100, 5)))



  my_summary <- summary(Species ~ ., data = iris, method = "reverse")
 fix_to_df(my_summary)


data(infert, package = "datasets")
infert$case  <- factor(infert$case ,1:0, c("case", "control") )

infert$spontaneous <- factor(infert$spontaneous)
infert$induced2    <- factor(infert$induced==0)
tab_1<- xtabs(~  case, infert)
tab_2x2<- xtabs(~ induced2 + case, infert)
tab_3x2<- xtabs(~ induced + case, infert)
tab_3x3<- xtabs(~ induced + education, infert)
tab_3x3x2<- xtabs(~ induced + education+case, infert)
tab_3x2x3<- xtabs(~ induced +case+ education, infert)
fix_to_df(tab_1)
 # 2x2
tab_2x2
fix_to_df(tab_2x2)
#fix_to_df(ftable(tab_2x2))
 # 3x2
tab_3x2
fix_to_df(tab_3x2)
#fix_to_df(ftable(tab_3x2))
 # 3x3
tab_3x3
fix_to_df(tab_3x3)
#fix_to_df(ftable(tab_3x3))
 # 3x3x2
tab_3x3x2
fix_to_df(tab_3x3x2)
#fix_to_df(ftable(tab_3x3x2))

 # 3x2x3
tab_3x2x3
# ftable(tab_3x2x3)
# as.data.frame(ftable(tab_3x2x3)) |>
#   Wide(induced  +  case ~education )
fix_to_df(tab_3x2x3)

}
