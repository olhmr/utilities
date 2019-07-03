context("Clean names")
library(utilities)

test_that("each basic replacement works", {
  # Capitals
  expect_equal(clean_names(c("ABC")),
               "abc")
  # Spaces
  expect_equal(clean_names(c("a b c")),
               c("a_b_c"))
  # Dashes
  expect_equal(clean_names(c("a-b-c")),
               c("a_b_c"))
  # Pluses
  expect_equal(clean_names(c("a+b+c")),
               c("a_b_c"))
  # Ampersands
  expect_equal(clean_names(c("a&b&c")),
               c("a_and_b_and_c"))
  # Percent (with space before)
  expect_equal(clean_names(c("a %")),
               c("a_percent"))
  # Percent (no space before)
  expect_equal(clean_names(c("a%")),
               c("a_percent"))
  # Multiple underscores
  expect_equal(clean_names(c("a__b___c")),
               c("a_b_c"))
  # Trailing underscores
  expect_equal(clean_names(c("abc_")),
               c("abc"))
  # Unacceptable leading character
  expect_equal(clean_names(c("1abc")),
               c("x1abc"))
  expect_equal(clean_names(c("@a")),
               c("x.a"))
})
test_that("multiple underscores are reduced to one", {
  expect_equal(clean_names(c("a___b")), "a_b")
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
