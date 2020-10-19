library(data.table)

context("Ordinal data")

test_that("K-alpha", {
  expect_error(kalpha(
    dt = nominal,
    unit = "unit",
    measurement = "measurement",
    level = "ordinal"
  ))
})
