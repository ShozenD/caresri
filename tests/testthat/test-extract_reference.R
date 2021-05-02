test_that("extract_reference() extracts the reference term", {
  x <- list(age = c("10~20","20~30","30+"), sex = c("Male", "Female"))
  z <- list(age = "10~20", sex = "Male")
  expect_equal(extract_reference(x), z)
})
