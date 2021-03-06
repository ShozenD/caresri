% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_samadult.R
\name{read_samadult}
\alias{read_samadult}
\title{Read a single NHIS sample adult file}
\usage{
read_samadult(
  file.path = NULL,
  year = NULL,
  lookup.tbl.path = NULL,
  rename = FALSE
)
}
\arguments{
\item{file.path}{Path to a sample adult file}

\item{year}{The year of the survey}

\item{lookup.tbl.path}{The path to a variable name look-up table}

\item{rename}{Rename specified variables to user defined names}
}
\value{
A dataframe with only the specified NHIS variables
}
\description{
Read a single NHIS sample adult file
}
\examples{
\dontrun{
df.nhis <- read_samadult("samadult_2017.csv", year = 2017, lookup.tlb.path = "samadult-lookup.csv")
}
}
