# TODO Document this function.

# TODO Remove massive duplication between decisiveness
#   and surrogacy and accuracy and replicability
#   (all the pre-formatting cruft).

# TODO Refactor replicability to use accuracy.
decisiveness <- function(coders,
                         standard,
                         unit_from = "unit",
                         measurement_from = "measurement",
                         frequency_from = NULL,
                         return_by_unit = FALSE) {
  # Change the names of the columns for readability.
  # TODO: might rename N to freq
  normalize_names <- function(dt) {
    setnames(dt,
      old = c(unit_from, measurement_from),
      new = c("unit", "measurement")
    )

    setnames(dt, old = frequency_from, new = "N")
  }
  normalize_names(coders)
  normalize_names(standard)

  # TODO: remove this ugly hack once check_and_set_keys and
  # compute_frequencies do not require a field name anymore.
  if (!is.null(frequency_from)) {
    frequency_from <- "N"
  }

  # N <- NULL # due to NSE notes in R CMD check # nolint

  coders <- check_and_set_keys(coders, "unit", "measurement")
  standard <- check_and_set_keys(standard, "unit", "measurement")

  # TODO: possible improvement, rbind earlier and compute frequencies
  # grouping by provenance, rather than duplicating calls.
  coders_freq <- compute_frequencies(
    coders,
    "unit",
    "measurement",
    frequency_from
  )

  std_freq <- compute_frequencies(
    standard,
    "unit",
    "measurement",
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

  # TODO: write test when there is a tie. The above
  # coud should give a count of N = 2.

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
