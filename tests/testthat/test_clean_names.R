context("Clean names")
library(utilities)

test_that("multiple underscores are reduced to one", {
  expect_equal(clean_names(c("a___")), "a_")
  expect_equal(clean_names(c("and___and_and__and___and")), "and_and_and_and_and")
})
test_that("an underscore is added before percent where appropriate", {
  expect_equal(clean_names(c("and 10%")), "and_10_percent")
  expect_equal(clean_names(c("a_%")), "a_percent")
})
test_that("data frame and vector inputs have same result", {
  test_data <- data.frame(`a+b` = c(1, 2, 3), `c&d%` = c(3, 4, 5))
  expect_equal(names(clean_names(test_data)), clean_names(names(test_data)))
})
