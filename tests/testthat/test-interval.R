library(data.table)

context("Interval data")

test_that("Replicability", {
  expect_error(replicability(
    dt = nominal,
    unit = "unit",
    measurement = "measurement",
    level = "interval"
  ))
})
