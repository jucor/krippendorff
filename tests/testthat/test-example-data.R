context("Presence of example data")


test_that("Hayes 2007 Table 1 is present and of right size",
          {
            expect_is(news.tone.wide, "data.table")
            expect_equal(ncol(news.tone.wide), 6)
            expect_equal(nrow(news.tone.wide), 40)

            expect_is(news.tone, "data.table")
            expect_equal(ncol(news.tone), 3)
            expect_equal(nrow(news.tone), 159)
          })


test_that("Krippendorff 2004 Section 11.3.1 is present and of right size",
          {
            expect_is(news.presence.wide, "data.table")
            expect_equal(ncol(news.presence.wide), 3)
            expect_equal(nrow(news.presence.wide), 10)

            expect_is(news.presence, "data.table")
            expect_equal(ncol(news.presence), 3)
            expect_equal(nrow(news.presence), 20)
          })
