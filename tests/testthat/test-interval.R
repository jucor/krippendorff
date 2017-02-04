library(data.table)

context("Interval data")

test_that("K-alpha", {
          expect_error(kalpha(DT = nominal,
                              unit = "unit",
                              measurement = "measurement",
                              level = "interval"))
          })
