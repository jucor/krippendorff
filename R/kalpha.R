#' Turn an observation data.frame into a long data.table
#'
#' @param data `data.table` containing the reliability data in wide format
#' @param unit Name of the column containing the unit ID
#' @param observers List of names of the columns containing the oberverments, one per observer
#'
#' @return A long-form melted data.table with the same unit column and two new columns "observer" and "measurement"
#'     and as many columns as units.
to.long <- function(data, unit, observers) {
  data.table::melt(data, id.vars = unit,
                   measure.vars = observers,
                   value.name = "measurement",
                   variable.name = "observer")
}

#' Compute Krippendorff's Alpha
#'
#' This function implements the computation of  Krippendorff's Alpha as per
#' http://web.asc.upenn.edu/usr/krippendorff/mwebreliability5.pdf
#'
#' It is designed to be space efficient for sparse oberverments, and as thus does
#' not take as input a reliability matrix, but a long-format data.table
#' TODO(jucor): cite properly
#'
#' @param data `data.table` containing the reliability data in long format
#' @param unit Name of the column containing the unit ID
#' @param measurement Name of the column containing the measurements, one per judge
#' @param level c('nominal', 'ordinal'): type of oberverment data.
kalpha <- function(data, unit, measurement, level) {
  #  data is a data.table
  # TODO(jucor): add default 'nominal'
  stopifnot(level == 'nominal')

  setkeyv(data, c(unit, measurement))

  values.by.unit <- data[,.N, by=c(unit, measurement)]
  print(values.by.unit)
  values.by.unit.matrix <- as.matrix(dcast(values.by.unit,
                                           paste(unit, "~", measurement),
                                           value.var = "N",
                                           fill = 0)[, -"Article"])
  print(values.by.unit.matrix)

  values.by.unit.matrix
}
