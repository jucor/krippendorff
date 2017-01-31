context("K-alpha without bootstrapping")

test_that("K-alpha on book section 11.3.1", {
          expect_equal(kalpha(krippendorff2004.binary.two.observers,
                             judges = list("Jon", "Han"),
                             level = "binary"),
                       0.095)
          })
