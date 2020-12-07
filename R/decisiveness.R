# TODO(julien) Document this function.
# TODO(julien) Refactor replicability to use accruacy.
decisiveness <- function(coders,
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

  # Compute majorities of coders per unit, including ties
  # coders <- example$coders # TODO(remove once teted!!)
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
  # should give a count of N = 2.

  # Compare majorities to standard
  acc <- accuracy(coders_freq,
    majority,
    frequency_from = "N",
    return_by_unit = return_by_unit
  )

  dec <- acc
  dec$decisiveness <- acc$accuracy
  dec$accuracy <- NULL

  return(dec)
}
