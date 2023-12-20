oe_data <- open_elections_factory("wi")
data <- generate_data(oe_data)
full_votes <- year_baseline_data(2010, data)



test_that("efficiency gap is in range", {
  expect_gte(efficiency_gap(full_votes, 2010), -0.4)
})

test_that("efficiency gap is in range", {
  expect_lte(efficiency_gap(full_votes, 2010), 0.4)
})

test_that("efficiency gap contested is in range", {
  expect_gte((efficiency_gap_contested(full_votes, 2010)), -0.4)
})

test_that("efficiency gap contested is in range", {
  expect_lte((efficiency_gap_contested(full_votes, 2010)), 0.4)
})
