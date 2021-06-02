test_that("read_samadult works", {
  # Default
  df <- suppressMessages(read_samadult(file.path = "test-data/samadult_2016.csv",
                                 year = 2016,
                                 lookup.tbl.path = "test-data/samadult-lookup.csv"))
  expect_true(is.data.frame(df))
  # Rename
  df <- suppressMessages(read_samadult(file.path = "test-data/samadult_2016.csv",
                                 year = 2016,
                                 lookup.tbl.path = "test-data/samadult-lookup.csv",
                                 rename = T))
  expect_true(is.data.frame(df))
})

test_that("read_samadult_at works", {
  # Default
  df <- suppressMessages(read_samadult_at(dir.path = "test-data",
                                   years = c(2016, 2017, 2018),
                                   lookup.tbl.path = "test-data/samadult-lookup.csv"))
  expect_true(is.data.frame(df))
  # Rename
  df <- suppressMessages(read_samadult_at(dir.path = "test-data",
                                   years = c(2016, 2017, 2018),
                                   lookup.tbl.path = "test-data/samadult-lookup.csv",
                                   rename = F))
  expect_true(is.data.frame(df))
})
