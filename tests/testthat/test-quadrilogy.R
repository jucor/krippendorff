library(data.table)

context("Quadrilogy on aggregated data")

# Reproduce Table 6 of Krippendorff (2020) quadrilogy draft
# TODO(julien): extract into a dataset for the package.
generate_example <- function() {
  coders <- matrix(c(1, 9, 0, 5, 8, 8, 0, 12, 6, 1),
    nrow = 2, ncol = 5, byrow = TRUE,
    dimnames = list(0:1, 1:5)
  )
  coders <- as.data.table(coders,
    keep.rownames = "value",
    value.name = "count"
  )
  coders <- data.table::melt(coders,
    id.vars = "value",
    variable.name = "unit",
    value.name = "count"
  )


  standard <- matrix(c(0, 1, 1, 0, 0, 2, 0, 1, 2, 0),
    nrow = 2, ncol = 5, byrow = TRUE,
    dimnames = list(0:1, 1:5)
  )
  standard <- as.data.table(standard,
    keep.rownames = "value",
    value.name = "count"
  )
  standard <- data.table::melt(standard,
    id.vars = "value",
    variable.name = "unit",
    value.name = "count"
  )

  return(list(standard = standard, coders = coders))
}


test_that("Replicability on aggregated binary data.table", {
  dt <- generate_example()$coders

  expect_equal(
    replicability(
      copy(dt[as.numeric(unit) %in% c(2, 3)]),
      "unit", "value", "count"
    )$alpha,
    1,
    tolerance = 1e-6
  )

  expect_equal(
    replicability(
      copy(dt[as.numeric(unit) %in% c(2, 3, 5)]),
      "unit", "value", "count"
    )$alpha,
    0.8687782805
  )

  expect_equal(
    replicability(
      copy(dt[as.numeric(unit) %in% c(1, 2, 3, 5)]),
      "unit", "value", "count"
    )$alpha,
    0.7989417989
  )


  r <- replicability(
    coders = copy(dt),
    unit_from = "unit",
    measurement_from = "value",
    frequency_from = "count"
  )
  expect_equal(
    r$alpha,
    0.60547504030
  )
})


test_that("Accuracy on aggregated binary data.table", {
  example <- generate_example()
  acc <- accuracy(
    coders = example$coders,
    standard = example$standard,
    unit_from = "unit",
    measurement_from = "value",
    frequency_from = "count"
  )
  expect_equal(
    acc$accuracy,
    0.369,
    tolerance = 1e-3
  )
})


test_that("Surrogacy on aggregated binary data.table", {
  example <- generate_example()
  surr <- surrogacy(
    coders = copy(example$coders),
    standard = copy(example$standard),
    unit_from = "unit",
    measurement_from = "value",
    frequency_from = "count",
    return_by_unit = TRUE
  )
  expect_equal(
    surr$surrogacy,
    0.656,
    tolerance = 1e-3
  )
})


test_that("Decisiveness on aggregated binary data.table", {
  example <- generate_example()
  dec <- decisiveness(
    coders = example$coders,
    unit_from = "unit",
    measurement_from = "value",
    frequency_from = "count"
  )
  expect_equal(
    dec$decisiveness,
    0.714,
    tolerance = 1e-3
  )
})


test_that("Decisiveness excludes units with a single coder", {
    coders <- data.table(data.frame(
      unit = c(1, 2, 2),
      coder = c("Alice", "Alice", "Bob"),
      value = c(1, 1, 0)))

    dec <- decisiveness(
      coders = copy(coders),
      unit_from = "unit",
      measurement_from = "value"
    )

    filtered <- coders[unit != 1]
    dec_filtered <- decisiveness(
      coders = copy(filtered),
      unit_from = "unit",
      measurement_from = "value"
    )


    expect_equal(
      dec$decisiveness,
      dec_filtered$decisiveness,
      tolerance = 1e-3
    )
})
