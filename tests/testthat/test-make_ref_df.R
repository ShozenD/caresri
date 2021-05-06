test_that("make_ref_df() creates a reference data frame", {
  df <- dplyr::tibble(
    male_under_20 = c(rep(1,10), rep(0,10)),
    age = c(rep("20<",10),rep("20+",10)),
    sex = c(rep("Male",10), rep("Female", 10))
  )

  m <- glm(male_under_20 ~ ., data=df, family=binomial)
  make_ref_df(m)

  y <- dplyr::tibble(
    term = c("age [Ref: 20+]", "sex [Ref: Female]"),
    estimate = rep(1,2),
    conf.low = rep(1,2),
    conf.high = rep(1,2),
    label = c("age","sex"),
    inner.order = rep(0,2)
  )

  expect_equal(make_ref_df(m), y)
})
