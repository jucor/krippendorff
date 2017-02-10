library(data.table)

context("Nominal data")


test_that("K-alpha on partial nominal data", {
  results <- kalpha(DT = mwebreliability5,
                              unit = "unit",
                              measurement = "measurement",
                              level = "nominal")
  expect_equal(results$alpha,
               0.743,
               tolerance = 1e-3)
})

