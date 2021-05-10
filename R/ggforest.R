#' Creates a forest plot
#'
#' @param model A fitted logistic regression model object
#' @param include.intercept Whether to include the model intercept within the plot
#' @param categories A named list containing the grouping of terms (e.g. list(Race = c("RACE"), SocioDemographics = c("AGE", "SEX")))
#' @param color.scheme A named list containing the coloring for each category
#' @param title Plot title
#' @param xlab x label
#' @param ylab y label
#' @param errorbar Whether to draw errorbars
#' @param errorbar.width The width of the errorbars
#' @param linetype The linetype of the reference line
#' @param point.size Point size
#' @param point.alpha Alpha value of the points
#' @param ggtheme ggplot theme
#' @param ... Other parameters passed to ggplot
#'
#' @return A forest plot
#' @export
#'
#' @examples
#' model <- glm(y ~ x, data=data, family=binomial)
#' ggforest(model)
ggforest <- function(model,
                     include.intercept = FALSE, categories = NULL, color.scheme = NULL,
                     title = NULL, xlab = "Terms", ylab = "Log Odds",
                     errorbar = TRUE, errorbar.width = 0.3,
                     linetype = "dashed", point.size = 3, point.alpha = 1.0,
                     ggtheme = theme_classic(),...)

{
  if(attr(model,"class")[1] == "glm"){
    message("Re-fitting the model using speedglm to speed up result computation")
    model <- speedglm::speedglm(model$formula,
                            data = model$data,
                            family = binomial()) # TODO: Make it such that this can handle other link functions as well
  }

  # Default options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .opts <- list(
    model = model, title = title, xlab = xlab, ylab = ylab,
    include.intercept = include.intercept, categories = categories, color.scheme = color.scheme,
    errorbar = errorbar, errorbar.width = errorbar.width,
    linetype = linetype, point.size = point.size,
    ggtheme = ggtheme, ...)

  p <- do.call(ggforest_core, .opts)

  return(p)
}

ggforest_core <- function(model,
                          include.intercept = FALSE, categories = NULL, color.scheme = NULL,
                          title = NULL, xlab = NULL, ylab = NULL,
                          errorbar = TRUE, errorbar.width = 0.3,
                          linetype = "dashed", point.size = 5, point.alpha = 1.0,
                          ggtheme = theme_classic(),...)
{
  df <- broom::tidy(model, conf.int = T, exponentiate = T) %>%
    dplyr::select(term, estimate, dplyr::starts_with("conf"))

  # Remove Intercept
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if (!include.intercept) {
    df <- filter(df, term != "(Intercept)")
  }

  # Remove term labels from subcategories
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  term.labels <- attr(model$terms, "term.labels")
  term.classes <- attr(model$terms, "dataClasses")
  term.labels.categorical <- term.labels[term.classes[-1] %in% c("character", "factor")]
  df <- mutate(df,
               label = str_match(term, paste(term.labels, collapse = "|")),
               term = str_remove_all(term, paste(term.labels.categorical, collapse = "|")))

  # Make references and order labels
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  df <- df %>%
    group_by(label) %>%
    mutate(inner.order = row_number()) %>%
    bind_rows(make_ref_df(model)) %>%
    mutate(label = factor(label, levels = term.labels)) %>%
    arrange(label, inner.order) %>%
    ungroup()

  df <- mutate(df,
               outer.order = row_number(),
               outer.order = factor(outer.order, levels = rev(outer.order)))

  # If specified, group terms into larger categories
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  if(is.list(categories)){
    df <- map_categories(df, categories)
  } else {
    df$category <- df$label
  }

  # Main plot
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  p <- ggplot(df, aes(y = estimate, x = outer.order, color=category))

  p <- p +
    geom_hline(yintercept = 1, linetype = linetype) +
    geom_point(size = point.size, alpha = point.alpha) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = errorbar.width)

  p <- p +
    scale_y_log10() +
    scale_x_discrete(labels = rev(df$term)) +
    coord_flip()

  if(!is.null(color.scheme)){
    p <- p + scale_color_manual(values = color.scheme)
  }

  p <- ggpubr::ggpar(p, ggtheme = ggtheme,
                     title = title, xlab = xlab, ylab = ylab, ...)

  return(p)
}

cat <- list(
  Race = c("RACE"),
  SocioDemographics = c("SEX", "AGE", "EDUC", "FAMINC", "MARITAL", "PLBORN"),
  HealthStatus = c("LIMIT"),
  LifeStyle = c("MD"),
  HealthCare = c("PHSTAT", "COVERAGE", "GMC"),
  Year = c("YEAR")
)

cs <- list(
 Race =  "#ffa600",
 SocioDemographics = "#ff6e54",
 HealthStatus = "#dd5182",
 LifeStyle = "#955196",
 HealthCare = "#444e86",
 Year = "#003f5c"
)
