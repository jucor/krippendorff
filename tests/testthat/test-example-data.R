context("Presence of example data")


test_that("Hayes 2007 Table 1 is present and of right size", {
  expect_is(news_tone_wide, "data.table")
  expect_equal(ncol(news_tone_wide), 6)
  expect_equal(nrow(news_tone_wide), 40)

  expect_is(news_tone, "data.table")
  expect_equal(ncol(news_tone), 3)
  expect_equal(nrow(news_tone), 159)
})


test_that("Krippendorff 2004 Section 11.3.1 is present and of right size", {
  expect_is(news_presence_wide, "data.table")
  expect_equal(ncol(news_presence_wide), 3)
  expect_equal(nrow(news_presence_wide), 10)

  expect_is(news_presence, "data.table")
  expect_equal(ncol(news_presence), 3)
  expect_equal(nrow(news_presence), 20)
})
