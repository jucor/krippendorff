library(data.table)

context("Helper functions")

test_that("Melt works as expected", {
          expected = news.presence
          obtained = to.long.form(DT = news.presence.wide,
                             unit = "article",
                             observers = c("jon", "han"),
                             measurements = "presence")
          setorder(expected, article, observer)
          setorder(obtained, article, observer)
          expect_equal(obtained, expected)
        })


test_that("Do not export private functions",
          {
            skip("TODO(jucor): enable Roxygen2")
            expect_error(.possible.disagreements)
          })
