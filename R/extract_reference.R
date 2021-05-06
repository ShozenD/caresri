#' Extract reference labels
#'
#' Extracts the reference term for each variable in a logistic regression.
#'
#' @param x A list containing the variables and their terms
#'
#' @return y A list containing the extracted reference term
#' @export
#'
#' @examples
extract_reference <- function(x) {
  labs <- names(x)
  y <- c()
  for (i in 1:length(x)) {
    y[[labs[i]]] <- paste0(labs[i]," [Ref: ",x[[i]][1],"]")
  }
  return(y)
}
