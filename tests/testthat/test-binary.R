library(data.table)

context("Binary data")


test_that("Replicability on full binary data.table", {
  expect_equal(replicability(
    dt = news_presence,
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
    dt = df,
    unit_from = "article",
    measurement_from = "presence"
  )$alpha,
  .0952,
  tolerance = 1e-3
  )
})


test_that("Replicability on aggregated binary data.table", {
  # Reproduce Table 6 of Krippendorff (2020) quadrilogy draft
  coders <- matrix(c(1, 9, 0, 5, 8, 8, 0, 12, 6, 1),
    nrow = 2, ncol = 5, byrow = TRUE,
    dimnames = list(0:1, 1:5)
  )
  dt <- as.data.table(coders,
    keep.rownames = "value",
    value.name = "count"
  )
  dt <- melt(dt,
    id.vars = "value",
    variable.name = "unit",
    value.name = "count"
  )

  expect_equal(
    replicability(
      copy(dt[as.numeric(unit) %in% c(2, 3)]),
      "unit", "value", "count"
    )$alpha,
    1,
    tolerance = 1e-4
  )

  expect_equal(
    replicability(
      copy(dt[as.numeric(unit) %in% c(2, 3, 5)]),
      "unit", "value", "count"
    )$alpha,
    0.870,
    tolerance = 1e-4
  )

  expect_equal(
    replicability(
      copy(dt[as.numeric(unit) %in% c(1, 2, 3, 5)]),
      "unit", "value", "count"
    )$alpha,
    0.799,
    tolerance = 1e-4
  )


  r <- replicability(
    dt = copy(dt),
    unit_from = "unit",
    measurement_from = "value",
    count_from = "count"
  )
  expect_equal(r$alpha,
    0.606,
    tolerance = 1e-4
  )
})
