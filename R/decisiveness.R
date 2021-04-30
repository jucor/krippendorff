#' Compute Krippendorff's Decisiveness
#'
#' Similar input and warnings as [replicability()].
#' TODO(julien): figure out how to merge both functions in the same help page.
#'
#' @param coders `data.table` containing the coders.
#' @param unit_from Name of the column containing the unit ID
#' @param measurement_from Name of the column containing the measurements
#' @param frequency_from (Optional) Name of the column containing the
#'   frequencies, *if* the data is in the "aggregated" form described above.
#' @param return_by_unit (default FALSE) If TRUE, return a data.table of
#' per-unit contributions.
#' @return
#' \item{decisiveness}{Krippendorff's Alpha decisiveness index}
#' \item{De}{Expected disagreement}
#' \item{Do}{Overall observed disagreement accross all units}
#' \item{by_unit}{(Only if return_by_unit = TRUE) Per-unit contributions}
#' @export
#' @import data.table
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
  # Two-step way to compute the argmax, as argmax is
  # not yet optimised in data.table
  coders_freq[,
    `:=`(
      unit_max = max(N),
      mu = sum(N)
    ),
    by = unit
  ]

  # Drop all units where there is only one grade
  coders_freq <- coders_freq[mu > 1]

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
