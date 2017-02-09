library(data.table)

context("Nominal data")


test_that("K-alpha on partial nominal data", {
          expect_equal(kalpha(DT = mwebreliability5,
                              unit = "unit",
                              measurement = "measurement",
                              level = "nominal")$alpha,
                       0.743,
                       tolerance = 1e-3)
          })

