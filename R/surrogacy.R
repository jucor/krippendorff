surrogacy <- function(coders,
                      standard,
                      unit_from = "unit",
                      measurement_from = "measurement",
                      frequency_from = NULL,
                      return_by_unit = FALSE) {
  coders_freq <- clean_and_count(
    coders,
    unit_from,
    measurement_from,
    frequency_from
  )

  std_freq <- clean_and_count(
    standard,
    unit_from,
    measurement_from,
    frequency_from
  )

  # Compute majorities of coders per unit, including ties
  # coders <- example$coders # TODO(remove once tested!!)
  # Two-step way to compute the argmax, as argmax is
  # not yet optimised in data.table
  coders_freq[,
    `:=`(
      unit_max = max(N),
      mu = sum(N)
    ),
    by = unit
  ]

  majority <- coders_freq[N == unit_max,
    .(measurement, N = mu / .N),
    by = unit
  ]

  # TODO(julien): write test when there is a tie. The above
  # coud should give a count of N = 2.

  # Compare majorities to standard
  acc <- accuracy(majority,
    std_freq,
    frequency_from = "N",
    return_by_unit = return_by_unit
  )

  surr <- acc
  surr$surrogacy <- acc$accuracy
  surr$accuracy <- NULL

  return(surr)
}
