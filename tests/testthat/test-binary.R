library(data.table)

context("K-alpha without bootstrapping")

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


test_that("K-alpha on partial nominal data", {
          expect_equal(kalpha(DT = nominal,
                              unit = "unit",
                              measurement = "measurement",
                              level = "nominal"),
                       0.743,
                       tolerance = 1e-3)
          })


test_that("K-alpha on full binary data", {
          expect_equal(kalpha(DT = news.presence,
                              unit = "article",
                              measurement = "presence",
                              level = "nominal"),
                       .0952,
                       tolerance=1e-3)
          })


test_that("Do not export private functions",
          {
            skip("TODO(jucor): enable Roxygen2")
            expect_error(.possible.disagreements)
          })
