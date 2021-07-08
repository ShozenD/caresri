#' @include utilities.R
#' @importFrom readr read_csv
#' @importFrom stringr str_glue
NULL
#' @title Read a single NHIS sample adult file
#'
#' @param file.path Path to a sample adult file
#' @param year The year of the survey
#' @param lookup.tbl.path The path to a variable name look-up table
#' @param rename Rename specified variables to user defined names
#'
#' @return A dataframe with only the specified NHIS variables
#' @export
#'
#' @examples
#' \dontrun{
#' df.nhis <- read_samadult("samadult_2017.csv", year = 2017, lookup.tlb.path = "samadult-lookup.csv")
#' }
read_samadult <- function(
  file.path = NULL,
  year = NULL,
  lookup.tbl.path = NULL,
  rename = FALSE
){
  ### Error Handling ###
  if(is.null(file.path)) stop("Path to file not specified (file.path)")
  if(is.null(year)) stop("Please specify the survey year")
  if(is.null(lookup.tbl.path)) stop("Path to variable look-up table not specified (lookup.tbl.path)")

  ### Look-up Table Handling ###
  lookup.tbl <- read_csv(lookup.tbl.path)

  if(year < 2013){
    .varnames <- lookup.tbl$names1
  } else if(year < 2016){
    .varnames <- lookup.tbl$names2
  } else {
    .varnames <- lookup.tbl$names3
  }

  ### Read NHIS file ###
  df <- read_csv(file.path) %>%
    dplyr::select(.varnames) %>%
    mutate(year = year)

  # TODO: Fix bug where it doesn't rename if there are no NA varnames
  if(rename){ # Rename columns to user specified names
    .newnames <- lookup.tbl$newnames
    .varnames <- .varnames[which(!is.na(.newnames))]
    .newnames <- .newnames[which(!is.na(.newnames))]

    df <- rename_at(df, .varnames, ~.newnames)
  } else {    # Rename columns to latest version
    df <- rename_at(df, .varnames, ~lookup.tbl$names3)
  }

  return(df)
}


#' Reads NHIS sample adult files for specified survey years
#'
#' @param dir.path Path to the directory(folder) that contains NHIS sample adult files
#' @param years A vector of years
#' @param lookup.tbl.path Path to the variable name look-up file
#' @param rename Rename column names to user specified names
#'
#' @return A NHIS dataframe with only the variables specified in the variable name look-up file
#' @export
#'
#' @examples
#' \dontrun{
#' read_samadult_at(dir.path = "./samadult-files",
#'                  years = c(2006,2012,2015),
#'                  lookup.tbl.path = "./samadult-lookup.csv")
#' }
read_samadult_at <- function(
  dir.path = NULL,
  years = NULL,
  lookup.tbl.path = NULL,
  rename = FALSE
){
  ### Handling Errors ###
  if(is.null(dir.path)) stop("Path to sample adult data directory is not specified")
  if(is.null(years)) stop("Please specify survey years")
  if(!is.vector(years) || !is.numeric(years)) stop("years must be a numeric vector")

  ### Load Data ###
  df <- purrr::map_dfr(years, ~{
    read_samadult(file.path = file.path(dir.path, str_glue("samadult_{year}.csv", year = .)),
                  year = .,
                  lookup.tbl.path = lookup.tbl.path,
                  rename = rename)
  })

  return(df)
}
