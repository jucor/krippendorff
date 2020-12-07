#' Compute Krippendorff's Accuracy
#'
#' Similar input and warnings as [replicability()].
#' TODO(julien): figure out how to merge both functions in the same help page.
#'
#' @param coders `data.table` containing the standards.
#' @param standard `data.table` containing the coders.
#' @param unit_from Name of the column containing the unit ID
#' @param measurement_from Name of the column containing the measurements
#' @param frequency_from (Optional) Name of the column containing the
#'   frequencies, *if* the data is in the "aggregated" form described above.
#' @param return_by_unit (default FALSE) If TRUE, return a data.table of
#' per-unit contributions.
#' @return
#' \item{accuracy}{Krippendorff's Alpha accuracy index}
#' \item{De}{Expected disagreement}
#' \item{Do}{Overall observed disagreement accross all units}
#' \item{by_unit}{(Only if return_by_unit = TRUE) Per-unit contributions}
#' @export
#' @import data.table
accuracy <- function(coders,
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


  std_freq[, std.sum := sum(N), by = unit]
  coders_freq[, coders.sum := sum(N), by = unit]

  prefix_with <- function(dt, prefix) {
    columns <- c("measurement", "N")
    setnames(dt,
      old = columns,
      new = paste0(prefix, ".", columns)
    )
  }
  prefix_with(coders_freq, "coders")
  prefix_with(std_freq, "std")


  # TODO(julien): see if filtering then matrix-multiplication is faster
  # than this join. Joining duplicates data, where matrix-mul
  # sums the units as we process rather than first computing the cartesian
  # product on the whole database. TODO(julien)
  joint <- coders_freq[std_freq, on = "unit", allow.cartesian = TRUE]

  # out_by_unit is an optional way to diagnose what each unit contributes
  if (return_by_unit) {
    out_by_unit <- joint[coders.sum > 0 &
      std.sum > 0,
    .(x = sum(coders.N * std.N / std.sum)),
    by = .(
      unit, std.measurement,
      coders.measurement
    )
    ]
  }

  out <- joint[coders.sum > 0 &
    std.sum > 0,
  .(x = sum(coders.N * std.N / std.sum)),
  by = .(std.measurement, coders.measurement)
  ]

  # Take the sum off-diagonal
  Do <- out[std.measurement != coders.measurement, sum(x)]

  coders_marginal <- out[,
    .(x.coders = sum(x), unit = "total"),
    by = .(coders.measurement)
  ]
  std_marginal <- out[,
    .(x.std = sum(x), unit = "total"),
    by = .(std.measurement)
  ]

  # TODO(jucor): simplify this using  matrices instead of
  # this wonky cartesian join on data.tables
  marginal_joint <- coders_marginal[std_marginal,
    .(unit,
      std.measurement,
      coders.measurement,
      prodx = x.std * x.coders
    ),
    on = "unit",
    allow.cartesian = TRUE
  ]

  De <- marginal_joint[std.measurement != coders.measurement, sum(prodx)] / out[, sum(x)]

  accuracy <- 1 - Do / De

  output <- list(
    accuracy = accuracy,
    Do = Do,
    De = De
  )

  if (return_by_unit) {
    output$by_unit <- out_by_unit
  }
  return(output)
}
