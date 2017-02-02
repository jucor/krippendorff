#' Turn an observation data.frame into a long data.table
#'
#' @param DT `data.table` containing the reliability data in wide format
#' @param unit Name of the column containing the unit ID
#' @param observers List of names of the columns containing the oberverments, one per observer
#' @param measurement Name of the new column containing the measurements
#'
#' @return A long-form melted data.table with the same unit column and two new columns "observer" and measurements
#'     and as many rows as units.
#' @export
to.long.form <- function(DT, unit, observers, measurements) {
  data.table::melt(DT, id.vars = unit,
                   measure.vars = observers,
                   value.name = measurements,
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
#' @export
# TODO(jucor): add default 'nominal'
# TODO(jucor): add alias 'binary' to 'nominal'
kalpha <- function(DT, unit, measurement, level) {
  stopifnot(level == 'nominal')
  stopifnot(is.data.table(DT))

  data.table::setkeyv(DT, c(unit, measurement))

  values.by.unit <- DT[, .N, by = c(unit, measurement)]
  # TODO(jucor): profile if doing it in two steps rather than having sum(N) twice speeds things up
  values.by.unit[,
                 nu := sum(N),
                 by=unit]
  ## Alternative, maybe more memory-efficient but makes the filtering to compute nc more awkard
  ## and thus probably slower (but profiling would be needed)
  # nu.table <- values.by.unit[,
  #                            .(nu = sum(N),
  #                              sufficient = sum(N) > 1),
  #                            by=unit]

  Do.by.unit <- values.by.unit[,
                               .(D = mismatchProbN(.SD$N)),
                               by=unit]

  # Omit all units with lone values
  nc <- values.by.unit[nu > 1,
                       .(N = sum(N)),
                       by=measurement]

  De <- mismatchProbN(nc[, N])

  1 - sum(Do.by.unit$D)/De
}


.onUnload <- function (libpath) {
  library.dynam.unload("krippendorff", libpath)
}
