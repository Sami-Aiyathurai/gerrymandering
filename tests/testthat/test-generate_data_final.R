oe_data <- open_elections_factory("wi")

test_that("generatedata returns 11 dataframes", {
  expect_equal(length(generate_data(oe_data)), 11)
})

'#access state year test: check column names
