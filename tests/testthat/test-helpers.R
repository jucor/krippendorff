library(data.table)

context("Helper functions")

test_that("Melt works as expected", {
  obtained <- to_long_form(
    dt = copy(news_presence_wide),
    unit = "article",
    observers = c("jon", "han"),
    measurements = "presence"
  )
  expected <- copy(news_presence)
  setorder(expected, article, observer)
  setorder(obtained, article, observer)
  expect_equal(obtained, expected)
})
