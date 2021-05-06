#' Creates a forest plot
#'
#' @param m A fitted logistic regression model object
#' @param intercept whether to include the intercept within the plot
#' @param ...
#'
#' @return A forest plot
#' @export
#'
#' @examples
#' model <- glm(y ~ x, data=data, family=binomial)
#' ggforest(model)
ggforest <- function(m,
                     intercept = FALSE,
                     ...)
{
  if(attr(m,"class")[1] == "glm"){
    message("Re-fitting the model using speedglm to speed up result computation")
    m <- speedglm::speedglm(m$formula,
                            data = m$data,
                            family = binomial()) # TODO: Make it such that this can handle other link functions as well
  }

  df <- broom::tidy(m, conf.int = T, exponentiate = T) %>%
    dplyr::select(term, estimate, dplyr::starts_with("conf"))

  # Remove Intercept
  if (!intercept) {
    df <- filter(df, term != "(Intercept)")
  }

  # Remove term labels
  term.labels <- attr(m$terms, "term.labels")
  term.classes <- attr(m$terms, "dataClasses")
  term.labels.categorical <- term.labels[term.classes[-1] == "character"]

  df <- mutate(df,
               label = str_match(term, paste(term.labels, collapse = "|")),
               label = factor(label, levels = term.labels),
               term = str_remove_all(term, paste(term.labels.categorical, collapse = "|")))

  df <- df %>%
    group_by(label) %>%
    mutate(inner.order = row_number()) %>%
    ungroup()

  df <- bind_rows(df, make_ref_df(m)) %>%
    group_by(label) %>%
    arrange(label, inner.order) %>%
    ungroup()

  df <- mutate(df,
               outer.order = row_number(),
               outer.order = factor(outer.order, levels = rev(outer.order)))

  # TODO: Make this more elegant!
  ggplot2::ggplot(df, aes(y = estimate, x = outer.order, color = label)) +
    geom_hline(yintercept = 1, linetype = 2) +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .2) +
    scale_y_log10() +
    scale_x_discrete(labels = rev(df$term)) +
    coord_flip() +
    labs(x = "Terms", y = expression(paste(log[e], "[OR]"))) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 9))
}
