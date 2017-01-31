context("Presence of example data")

test_that("Hayes 2007 Table 1 is present and of right size",
          {
            expect_is(hayes2007, "data.frame")
            expect_equal(ncol(hayes2007), 6)
            expect_equal(nrow(hayes2007), 40)
          })

test_that("Krippendorff 2004 Section 11.3.1 is present and of right size",
          {
            expect_is(krippendorff2004.binary.two.observers, "data.frame")
            expect_equal(ncol(krippendorff2004.binary.two.observers), 3)
            expect_equal(nrow(krippendorff2004.binary.two.observers), 10)
          })
