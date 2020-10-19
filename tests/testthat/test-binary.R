library(data.table)


context("Binary data")


test_that("K-alpha on full binary data", {
  expect_equal(kalpha(
    dt = news.presence,
    unit = "article",
    measurement = "presence",
    level = "binary"
  )$alpha,
  .0952,
  tolerance = 1e-3
  )
})
