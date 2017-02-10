library(data.table)

context("Boostrap")

test_that("Bootstrap on nominal data", {
  nboot = 1000
  mwebreliability5
  result <- kboot(DT = mwebreliability5, #,news.tone,
                         unit = "unit",
                         measurement = "measurement",
                         observer = "observer",
                         level = "nominal",
                         nboot = nboot)
  expect_equal(length(result$samples), nboot)
  expect_gt(result$ll95, 0)
  expect_gt(result$ul95, 0)
  expect_gt(result$ul95, result$ll95)
  # TODO(jucor): find a nominal dataset to bootstrap for which Krippendorff gives values that I can use as test
  # TODO(jucor): or implement ordinal level to use the values in Hayes 2007
})

test_that("Bootstrap with bad names", {
  nboot = 1000
  colnames(mwebreliability5) <- c("myunit", "myobs", "mymeasure")
  result <- kboot(DT = mwebreliability5, #,news.tone,
                         unit = "myunit",
                         measurement = "mymeasure",
                         observer = "myobs",
                         level = "nominal",
                         nboot = nboot)
  expect_equal(length(result$samples), nboot)
  expect_gt(result$ll95, 0)
  expect_gt(result$ul95, 0)
  expect_gt(result$ul95, result$ll95)
  # TODO(jucor): find a nominal dataset to bootstrap for which Krippendorff gives values that I can use as test
  # TODO(jucor): or implement ordinal level to use the values in Hayes 2007
})
