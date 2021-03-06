% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_categories.R
\name{map_categories}
\alias{map_categories}
\title{Given a dictionary of categories, this function will create a "category" column in the data frame,
which enables users color the forest plot not only by its variable name but also pre-defined categories}
\usage{
map_categories(df, x)
}
\arguments{
\item{df}{A dataframe containing model results}

\item{x}{A named list}
}
\value{
A dataframe
}
\description{
Given a dictionary of categories, this function will create a "category" column in the data frame,
which enables users color the forest plot not only by its variable name but also pre-defined categories
}
\examples{
df <- data.frame(variable = 1:12, label = c(rep("RACE",4),rep("FAMINC",5),rep("SEX",2),"COVERAGE"))
x <- list(SocioDemographics = c("SEX", "FAMINC"), "Health Care" = c("COVERAGE"), Race = c("RACE"))

map_categories(df, x)
}
