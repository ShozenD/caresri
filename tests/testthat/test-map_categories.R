test_that("map_categories() works", {
  df <- data.frame(
    variable = 1:12,
    label = c(rep("RACE",4),rep("FAMINC",5),rep("SEX",2),"COVERAGE")
  )

  x <- list(
    SocioDemographics = c("SEX", "FAMINC"),
    "Health Care" = c("COVERAGE"),
    Race = c("RACE")
  )

  y <- map_categories(df, x)

  z <- c(rep("Race",4),rep("SocioDemographics",7),"Health Care")

  expect_equal(as.character(y$category), z)
})
