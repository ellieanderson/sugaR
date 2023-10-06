# CREATE_TIME_DATA
# test
test_that("create_time_data is working as expected", {
  actual <- create_time_data("tests/testthat/df1_test.xlsx",
                             "tests/testthat/df2_test.xlsx",
                             "tests/testthat/df3_test.xlsx")

  id <- as.numeric(c(1, 2, 3, 1, 2, 3, 1, 2, 3))
  val <- as.numeric(seq(1:9))
  period <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)

  expected <- cbind(id, val, period) %>%
    as_tibble()

  expect_equal(actual, expected)
})
