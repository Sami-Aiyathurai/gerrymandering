library(tidyverse)
oe_data <- open_elections_factory("wi")

test_that("generatedata returns 11 dataframes", {
  expect_equal(length(generate_data(oe_data)), 11)
})

test_that("generatedata dataframes have 11 columns", {
  expect_equal(length(generate_data(oe_data)[[1]]), 11)
})

test_that("access_state_year has the same columns has generate data", {
  expect_equal(colnames(generate_data(oe_data)), colnames(generate_data(oe_data)))
})
