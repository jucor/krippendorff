library(data.table)

context("Bootstrap")

test_that("Bootstrap on nominal data", {
  skip("TODO(jucor): implement for more than 2 observers")
  nboot <- 1000
  mwebreliability5
  result <- kboot(
    dt = copy(mwebreliability5),
    unit = "unit",
    measurement = "measurement",
    observer = "observer",
    nboot = nboot
  )
  expect_equal(length(result$samples), nboot)
  expect_gt(result$ll95, 0)
  expect_gt(result$ul95, 0)
  expect_gte(result$ul95, result$ll95)
  # TODO(jucor): find a nominal dataset to bootstrap for which Krippendorff
  # gives values that I can use as test
  # TODO(jucor): or implement ordinal level to use the values in Hayes 2007
})

test_that("Bootstrap with non-standard names", {
  skip("TODO(jucor): implement for more than 2 observers")
  nboot <- 1000
  dt <- copy(mwebreliability5)
  colnames(dt) <- c("myunit", "myobs", "mymeasure")
  result <- kboot(
    dt = dt,
    unit = "myunit",
    measurement = "mymeasure",
    observer = "myobs",
    nboot = nboot
  )
  expect_equal(length(result$samples), nboot)
  expect_gt(result$ll95, 0)
  expect_gt(result$ul95, 0)
  expect_gt(result$ul95, result$ll95)
  # TODO(jucor): find a nominal dataset to bootstrap for which Krippendorff
  # gives values that I can use as test
  # TODO(jucor): or implement ordinal level to use the values in Hayes 2007
})

test_that("Bootstrap with only two observers", {
  nboot <- 1000
  dt <- data.table(
    unit = c(1, 1, 2, 2, 3, 3),
    observer = as.factor(c(1, 2, 1, 2, 1, 2)),
    measurement = c(0, 0, 1, 1, 0, 0)
  )
  result <- kboot(
    dt = dt,
    unit = "unit",
    measurement = "measurement",
    observer = "observer",
    nboot = nboot
  )
  expect_equal(length(result$samples), nboot)
  expect_gt(result$ll95, 0)
  expect_gt(result$ul95, 0)
  expect_gte(result$ul95, result$ll95)
  # TODO(jucor): find a nominal dataset to bootstrap for which Krippendorff
  # gives values that I can use as test
  # TODO(jucor): or implement ordinal level to use the values in Hayes 2007
})
