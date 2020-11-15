library(data.table)

context("Ordinal data")

test_that("Replicability", {
  expect_error(replicability(
    dt = nominal,
    unit = "unit",
    measurement = "measurement",
    level = "ordinal"
  ))
})
