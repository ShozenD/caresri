test_that("extract_reference() extracts the reference term", {
  x <- list(age = c("10~20","20~30","30+"), sex = c("Male", "Female"))
  z <- list(age = "age [Ref: 10~20]", sex = "sex [Ref: Male]")
  expect_equal(extract_reference(x), z)
})
