library(data.table)

context("Nominal data")


test_that("Replicability on partial nominal data.table", {
  results <- replicability(
    dt = mwebreliability5,
    unit_from = "unit",
    measurement_from = "measurement"
  )
  expect_equal(results$alpha,
    0.743,
    tolerance = 1e-3
  )
})


test_that("Replicability.df on partial nominal tibble", {
  df <- setDF(copy(mwebreliability5))

  results <- replicability(
    dt = df,
    unit_from = "unit",
    measurement_from = "measurement"
  )
  expect_equal(results$alpha,
    0.743,
    tolerance = 1e-3
  )
})
