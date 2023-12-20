library(tidyverse)
oe_data <- open_elections_factory("wi")


test_that("generatedata returns 11 dataframes", {
  expect_equal(length(sa_contest_all(generate_data(oe_data))), 11)
})

test_that("generatedata dataframes have 14 columns", {
  expect_equal(length(sa_contest_all(generate_data(oe_data))[[1]]), 14)
})
