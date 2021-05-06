#' Creates a tibble(data frame) with variables and their reference labels
#'
#' @param m A logistic regression model object
#'
#' @return A tibble(data frame) containing variable names and their reference labels
#' @export
#'
#' @examples
#' m <- glm(y ~ x, data=d, family=binomial)
#' make_ref_df(m)
make_ref_df <- function(m){
  y <- extract_reference(m$xlevels)

  N <- length(y)
  df <- dplyr::tibble(
    term = as.character(unlist(y)),
    estimate = rep(1,N),
    conf.low = rep(1,N),
    conf.high = rep(1,N),
    label = names(y),
    inner.order = rep(0,N)
  )
  return(df)
}
