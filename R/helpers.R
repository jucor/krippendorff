#' Turn an observation data.frame into a long data.table
#'
#' @param dt `data.table` containing the reliability data in wide format
#' @param unit name of the column containing the unit id
#' @param observers list of names of the columns containing the oberverments,
#'   one per observer
#' @param measurements name of the new column containing the measurements
#'
#' @return A long-form melted data.table with the same unit column and two new
#'   columns "observer" and measurements and as many rows as units.
#' @export
to_long_form <- function(dt, unit, observers, measurements) {
  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
  }
  data.table::melt(dt,
    id.vars = unit,
    measure.vars = observers,
    value.name = measurements,
    variable.name = "observer"
  )
}


#' Prepare and clean a data.table for quadrilogy
#'
#' Standardizes the names of the columns, set the keys,
#' compute the frequencies
#' @param dt `data.table` containing measurements, either coders or standard.
#' @param unit_from Name of the column containing the unit ID
#' @param measurement_from Name of the column containing the measurements
#' @param frequency_from (Optional) Name of the column containing the
#'   frequencies, *if* the data is in the "aggregated".
#' @return
clean_and_count <- function(dt,
                            unit_from,
                            measurement_from,
                            frequency_from) {
  # Support tidyverse by converting non-dt to dt
  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
  }

  # Change the names of the columns for readability.
  # TODO(julien): might rename N to freq
  setnames(dt,
    old = c(unit_from, measurement_from),
    new = c("unit", "measurement"),
    skip_absent = TRUE
  )


  # Compute reliability matrix, i.e.
  # frequencies of coders for each measurement's possible value for each unit
  # in the column "N"
  data.table::setkeyv(dt, c("unit", "measurement"))

  compute_frequency <- is.null(frequency_from)
  if (compute_frequency) {
    dt_freq <- dt[,
      .N,
      by = .(unit, measurement)
    ]
  } else {
    setnames(dt,
      old = c(frequency_from),
      new = c("N")
    )
    dt_freq <- dt
  }

  return(dt_freq)
}
