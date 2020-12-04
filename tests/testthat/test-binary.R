library(data.table)

context("Binary data")


test_that("Replicability on full binary data.table", {
  expect_equal(replicability(
    coders = news_presence,
    unit_from = "article",
    measurement_from = "presence"
  )$alpha,
  .0952,
  tolerance = 1e-3
  )
})


test_that("Replicability on full binary tibble", {
  df <- setDF(copy(news_presence))

  expect_equal(replicability(
    coders = df,
    unit_from = "article",
    measurement_from = "presence"
  )$alpha,
  .0952,
  tolerance = 1e-3
  )
})
