library(data.table)

context("Nominal data")


test_that("Replicability on partial nominal data.table", {
  results <- replicability(
    dt = mwebreliability5,
    unit = "unit",
    measurement = "measurement",
    level = "nominal"
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
    unit = "unit",
    measurement = "measurement",
    level = "nominal"
  )
  expect_equal(results$alpha,
    0.743,
    tolerance = 1e-3
  )
})
