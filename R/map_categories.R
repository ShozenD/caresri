#' Given a dictionary of categories, this function will create a "category" column in the data frame,
#' which enables users color the forest plot not only by its variable name but also pre-defined categories
#'
#' @param df
#' @param x
#'
#' @return A data frame
#' @export
#'
#' @examples
#' df <- data.frame(variable = 1:12, label = c(rep("RACE",4),rep("FAMINC",5),rep("SEX",2),"COVERAGE"))
#' x <- list(SocioDemographics = c("SEX", "FAMINC"), "Health Care" = c("COVERAGE"), Race = c("RACE"))
#'
#' map_categories(df, x)
#'
map_categories <- function(df, x){
  cat <- c()
  lab <- c()
  for(category in names(x)){
    labels <- x[[category]]
    cat <- c(cat, rep(category, length(labels)))
    lab <- c(lab, labels)
  }

  dplyr::mutate(df,
         category = as.character(label),
         category = plyr::mapvalues(category, from=lab, to=cat),
         category = factor(category, levels = names(x))
         )
}
