oe_data <- open_elections_factory("wi")

data <- generate_data(oe_data)

votes_2010 <- year_baseline_data(2010, data)

test_that("There are 4 columns", {
  expect_equal(ncol(votes_2010), (4))
})

test_that("There are 99 observations", {
  expect_equal(nrow(votes_2010), (99))
})
