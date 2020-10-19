library(data.table)

context("Helper functions")

test_that("Melt works as expected", {
  expected <- news.presence
  obtained <- to_long_form(
    dt = news.presence.wide,
    unit = "article",
    observers = c("jon", "han"),
    measurements = "presence"
  )
  setorder(expected, article, observer)
  setorder(obtained, article, observer)
  expect_equal(obtained, expected)
})
