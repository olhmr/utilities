context("Clean names")
library(utilities)

test_that("each basic replacement works", {
  # Upper case to lower case
  expect_equal(clean_names(c("ABC")),
               "abc")
  # Spaces to underscores
  expect_equal(clean_names(c("a b c")),
               c("a_b_c"))
  # Dashes to underscores
  expect_equal(clean_names(c("a-b-c")),
               c("a_b_c"))
  # Pluses to underscores
  expect_equal(clean_names(c("a+b+c")),
               c("a_b_c"))
  # Ampersands to "_and_"
  expect_equal(clean_names(c("a&b&c")),
               c("a_and_b_and_c"))
  # Percent (with underscore before) to "_percent"
  # (spaces will have changed to underscores from earlier)
  expect_equal(clean_names(c("a_%")),
               c("a_percent"))
  # Percent (no space before) to "_percent" (adding the underscore)
  expect_equal(clean_names(c("a%")),
               c("a_percent"))
  # Reduce multiple underscores to single
  expect_equal(clean_names(c("a__b___c")),
               c("a_b_c"))
  # Remove trailing underscores
  expect_equal(clean_names(c("abc_")),
               c("abc"))
  # Unacceptable leading character fixed by make.names
  expect_equal(clean_names(c("1abc")),
               c("x1abc"))
  expect_equal(clean_names(c("@a")),
               c("x.a"))
})
test_that("an underscore is added before percent where appropriate", {
  expect_equal(clean_names(c("and 10%")), "and_10_percent")
  expect_equal(clean_names(c("a_%")), "a_percent")
})
test_that("data frame and vector inputs have same result", {
  test_data <- data.frame(`a+b` = c(1, 2, 3), `c&d%` = c(3, 4, 5))
  expect_equal(names(clean_names(test_data)), clean_names(names(test_data)))
})
