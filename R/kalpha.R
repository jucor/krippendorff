#' Turn an observation data.frame into a long data.table
#'
#' @param DT `data.table` containing the reliability data in wide format
#' @param unit Name of the column containing the unit ID
#' @param observers List of names of the columns containing the oberverments,
#'   one per observer
#' @param measurements Name of the new column containing the measurements
#'
#' @return A long-form melted data.table with the same unit column and two new
#'   columns "observer" and measurements and as many rows as units.
#' @export
to.long.form <- function(DT, unit, observers, measurements) {
  data.table::melt(DT,
    id.vars = unit,
    measure.vars = observers,
    value.name = measurements,
    variable.name = "observer"
  )
}
#' Compute Krippendorff's Alpha
#'
#' This function implements the computation of  Krippendorff's Alpha as per
#' https://repository.upenn.edu/cgi/viewcontent.cgi?article=1043&context=asc_papers # nolint
#'
#' It is designed to be space efficient for sparse oberverments, and as thus
#' does not take as input a reliability matrix, but a long-format data.table
#' TODO(jucor): cite properly
#'
#' @param DT `data.table` containing the reliability data in long format
#' @param unit Name of the column containing the unit ID
#' @param measurement Name of the column containing the measurements, one per
#'   judge
#' @param level c('binary', 'nominal'): type of oberverment data.
#' @return
#' \item{alpha}{Krippendorff's Alpha reliability index}
#' \item{De}{Expected disagreement}
#' \item{Do}{Overall observed disagreement accross all units}
#' \item{by.unit}{Dataframe with one line per unit and columns}
#' \item{unit}{Unit}
#' \item{mu}{Number of observations in that unit}
#' \item{Do}{Observed disagreement within this unit}
#' @export
#' @import data.table
# TODO(jucor): add default 'nominal'
kalpha <- function(DT, unit, measurement, level) {
  . <- mu <- N <- NULL # due to NSE notes in R CMD check

  count <- switch(level,
    binary = countNominal,
    nominal = countNominal
  )
  if (is.null(count)) {
    stop("Level %s unknown, must be one of 'binary', 'nominal'")
  }

  stopifnot(is.data.table(DT))

  data.table::setkeyv(DT, c(unit, measurement))

  values.by.unit <- DT[, .N, by = c(unit, measurement)]

  # Compute one mu value per unit.
  by.unit <- values.by.unit[, .(
    Do = countNominal(.SD$N),
    mu = sum(.SD$N)
  ),
  by = unit
  ]

  # Compute one mu value per unit and repeat it for each measurement within the
  # unit
  values.by.unit[,
    mu := sum(N),
    by = unit
  ]

  # Omit all units with a single value
  nc <- values.by.unit[mu >= 2,
    .(N = sum(N)),
    by = measurement
  ]

  n <- nc[, sum(N)]
  De <- countNominal(nc[, N]) / n
  Do <- sum(by.unit$Do) / n

  alpha <- 1 - Do / De

  list(alpha = alpha, Do = Do, De = De, n = n, by.unit = by.unit)
}


#' Bootstrap Kalpha K times
#' This function implements the bootstrap of Krippendorff's Alpha as per
#' http://web.asc.upenn.edu/usr/krippendorff/boot.c-Alpha.pdf
#'
#' It is designed to be space efficient for sparse oberverments, and as thus
#' does not take as input a reliability matrix, but a long-format data.table
#' TODO(jucor): cite properly
#'
#' @param DT `data.table` containing the reliability data in long format
#' @param unit Name of the column containing the unit ID
#' @param observer Name of the column containing the observer name, one per
#'   judge
#' @param measurement Name of the column containing the measurements, one per
#'   judge
#' @param level c('binary', 'nominal'): type of oberverment data.
#' @param nboot number of samples alpha to bootstrap
#' @return \item{samples}{Vector of nboot sampled alphas}
#' \item{ll95}{Lower limit of 95\% CI interval}
#' \item{ul95}{Upper limit of 95\% CI interval}
#' \item{q.alphamin}{Probability of failure to achieve an alpha of at least
#' alphamin, dataframe with alphamin and q}
#' @import data.table
#' @importFrom stats ecdf quantile
#' @export
kboot <- function(DT, unit, observer, measurement, level, nboot) {
  . <- mu <- o1 <- o2 <- delta <- NULL # due to NSE notes in R CMD check
  m1 <- m2 <- e <- deviation <- alpha <- NULL # due to NSE notes in R CMD check

  if (!(level %in% c("binary", "nominal"))) {
    stop("Boostrap only implemented for binary and nominal data")
  }

  if (DT[, nlevels(observer)] != 2) {
    stop("Bootstrap currently only implemented for exactly 2 observers")
  }

  # compute expected disagreement from the main alpha
  point.estimate <- kalpha(DT, unit, measurement, level)
  De <- point.estimate$De
  N <- point.estimate$n
  N0 <- point.estimate$by.unit[, sum(.5 * (mu - 1) * mu)]


  # use a join to generate all pairs
  setkeyv(DT, c(unit))
  pairs <- DT[
    DT,
    allow.cartesian = TRUE
  ][, c(
    unit,
    observer,
    paste("i", observer, sep = "."),
    measurement,
    paste("i", measurement, sep = ".")
  ), with = FALSE]
  colnames(pairs) <- c("unit", "o1", "o2", "m1", "m2")

  # Order observers for simplicity
  pairs[, ":="(o1 = as.ordered(o1), # nolint
    o2 = as.ordered(o2))]

  # drop self-pairs
  pairs.not.self <- pairs[o1 < o2]


  # compute deviation E on all pairs
  # TODO(jucor): compute delta for ordinal and interval
  pairs.not.self[, delta := as.numeric(m1 != m2)]
  # For some unclear reason, we do not need the factor 2 present in
  # boot.c-Alpha.pdf
  # TODO(jucor): figure out why :p
  pairs.not.self[, e := delta / (N * De)]
  # join to get the mu for each pair of each unit
  setnames(point.estimate$by.unit, unit, "unit")
  pairs.with.mu <- point.estimate$by.unit[pairs.not.self, on = "unit"]
  deviations <- pairs.with.mu[, .(deviation = e / (mu - 1)), keyby = "unit"]



  # =============================
  # ALERT: stratifying by unit does not work for cases where there are only two
  # graders!! Sample the deviations -- that might be the slow part right there
  # TODO(jucor): potential speed-up by *not* bootstrapping the units within
  # which the deviation is constant (e.g. units where all graders agree on the
  # same grade)
  # sampled.deviations.by.unit <- deviations[,
  #   .(sample = 1:nboot, deviations = bootstrapWithinUnit(deviation, nboot)),
  #   keyby="unit"]
  # =============================


  # Since we know for now taht we only have two graders, hence mu=2 for all units, we can resample pairs accross units
  # without stratifying.
  # TODO(jucor): implement bootstrap for more than two graders
  sampled.deviations <- deviations[
    sample.int(.N, .N * nboot, replace = TRUE),
    .(deviation, sample = seq(1, nboot))
  ]


  # TODO(jucor): for the case where mu is constant throughout all units (e.g.
  # for 2 observers), sample from the much much smaller observed coincidence
  # matrix rather than from all the pairs.

  # And compute the final alphas, summing over the deviations for each unit
  samples <- sampled.deviations[, .(alpha = 1 - sum(deviation)), by = "sample"]

  alphamin <- seq(.5, .9, .1)
  q <- ecdf(samples[, alpha])(alphamin)
  ci <- quantile(samples[, alpha], c(.025, .975))
  list(
    ll95 = ci[1],
    ul95 = ci[2],
    q.alphamin = data.frame(alphamin, q),
    samples = samples[, alpha]
  )
}

.onUnload <- function(libpath) {
  library.dynam.unload("krippendorff", libpath)
}
