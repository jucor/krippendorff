#' Compute Krippendorff's Replicability (formerly Krippendorff Alpha)
#'
#' This function implements the computation of  Krippendorff's Alpha as per
#' https://repository.upenn.edu/cgi/viewcontent.cgi?article=1043&context=asc_papers # nolint
#'
#' It is designed to be space efficient for sparse oberverments, and as thus
#' does not take as input a reliability matrix, but a long-format data.table
#'
#' Supports nominal and binary data.
#'
#' If a tibble or a non-data.table dataframe is passed as input, this function
#' will still work! It will silently create a data.table copy of your dataframe
#' (or tibble).
#'
#' There are two possible types of input:
#' - The "extended" form: a tidy table of individual votes, where each row is
#' one measurement of one unit by one coder. This is typically the raw data

#' coming out of an annotation database. The user just needs to specify which
#' column contains the unit ID, and which column contains the measurement. Each
#' measurement takes one of the possible values of the nominal variable.
#' - The "aggregated" form: a tidy table of frequencies of votes per unit, where
#' each row is the number of measurements for one unit for one nominal value.
#' The user needs to specify which column contains the unit ID, which column
#' contains the measurement value (amongst one of the possible values of the
#' nominal variable), and which column contains the frequency of coders having
#' assigned this measurement to this unit.
#'
#' WARNING: this function *will* change the data.table in `dt`. If you want to
#' avoid this, better calling it on a copy of the table.
#'
#' @param coders `data.table` containing the reliability data in long format.
#' @param unit_from Name of the column containing the unit ID
#' @param measurement_from Name of the column containing the measurements
#' @param frequency_from (Optional) Name of the column containing the
#' frequencies, *if* the data is in the "aggregated" form described above.
#' @param return_by_unit (default FALSE) If TRUE, return a data.table of
#' @return
#' \item{alpha}{Krippendorff's Alpha reliability index}
#' \item{De}{Expected disagreement}
#' \item{Do}{Overall observed disagreement accross all units}
#' \item{by_unit}{Dataframe with one line per unit and columns}
#' \item{unit}{Unit}
#' \item{mu}{Number of observations in that unit}
#' \item{Do}{Observed disagreement within this unit}
#' @export
#' @import data.table
replicability <- function(coders,
                          unit_from = "unit",
                          measurement_from = "measurement",
                          frequency_from = NULL,
                          return_by_unit = FALSE) {
  mu <- N <- NULL # due to NSE notes in R CMD check # nolint

  # TODO(julien): add support for arbitrary difference functions beyond
  # counting nominal variables. Would allow reliability for ordinal and
  # for intervals.

  frequency_by_unit <- clean_and_count(
    coders,
    unit_from,
    measurement_from,
    frequency_from
  )

  # Compute one mu value per unit and recycle it for each measurement within the
  # unit. This recycling is why we create a variable by reference.
  frequency_by_unit[,
    mu := sum(N),
    by = unit
  ]


  # Sum frequencies over all units for each measurement, omitting units with a
  # single value (for which there cannot thus be any disagreement).
  nc <- frequency_by_unit[mu > 1,
    .(N = sum(N)),
    by = measurement
  ]

  n <- nc[, sum(N)]
  De <- count_disagreements(nc[, N]) / n # nolint

  # Compute disagreements and frequencies per unit.
  by_unit <- frequency_by_unit[,
    .(
      Do = count_disagreements(N),
      mu = sum(N)
    ),
    by = unit
  ]
  Do <- sum(by_unit$Do) / n # nolint

  alpha <- 1 - Do / De

  # TODO(jucor): rename alpha to "replicability" for consistency
  # with the three other alphas
  out <- list(alpha = alpha, Do = Do, De = De, n = n)
  if (return_by_unit) {
    out$by_unit <- by_unit
  }

  return(out)
}


#' Bootstrap replicability K times
#' This function implements the bootstrap of Krippendorff's Alpha as per
#' http://web.asc.upenn.edu/usr/krippendorff/boot.c-Alpha.pdf
#'
#' It is designed to be space efficient for sparse oberverments, and as thus
#' does not take as input a reliability matrix, but a long-format data.table
#' TODO(jucor): cite properly
#'
#' @param dt `data.table` containing the reliability data in long format
#' @param unit Name of the column containing the unit ID
#' @param observer Name of the column containing the observer name, one per
#'   judge
#' @param measurement Name of the column containing the measurements, one per
#'   judge
#' @param nboot number of samples alpha to bootstrap
#' @return \item{samples}{Vector of nboot sampled alphas}
#' \item{ll95}{Lower limit of 95\% CI interval}
#' \item{ul95}{Upper limit of 95\% CI interval}
#' \item{q.alphamin}{Probability of failure to achieve an alpha of at least
#' alphamin, dataframe with alphamin and q}
#' @import data.table
#' @importFrom stats ecdf quantile
#' @export
kboot <- function(dt, unit, observer, measurement, nboot) {
  mu <- o1 <- o2 <- delta <- NULL # due to NSE notes in R CMD check
  m1 <- m2 <- e <- deviation <- alpha <- NULL # due to NSE notes in R CMD check

  if (dt[, nlevels(observer)] != 2) {
    stop("Bootstrap currently only implemented for exactly 2 observers")
  }

  if (!is.data.table(dt)) {
    dt <- as.data.table(dt)
  }

  # compute expected disagreement from the main alpha
  point_estimate <- replicability(dt,
    unit,
    measurement,
    return_by_unit = TRUE
  )

  De <- point_estimate$De # nolint
  N <- point_estimate$n # nolint
  N0 <- point_estimate$by_unit[, sum(.5 * (mu - 1) * mu)] # nolint


  # use a join to generate all pairs
  setkeyv(dt, c(unit))
  pairs <- dt[
    dt,
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
  pairs_not_self <- pairs[o1 < o2]


  # compute deviation E on all pairs
  # TODO(jucor): compute delta for ordinal and interval
  pairs_not_self[, delta := as.numeric(m1 != m2)]
  # For some unclear reason, we do not need the factor 2 present in
  # boot.c-Alpha.pdf
  # TODO(jucor): figure out why :p
  pairs_not_self[, e := delta / (N * De)]
  # join to get the mu for each pair of each unit
  setnames(point_estimate$by_unit, unit, "unit")
  pairs_with_mu <- point_estimate$by_unit[pairs_not_self, on = "unit"]
  deviations <- pairs_with_mu[, .(deviation = e / (mu - 1)), keyby = "unit"]



  # =============================
  # ALERT: stratifying by unit does not work for cases where there are only two
  # graders!! Sample the deviations -- that might be the slow part right there
  # TODO(jucor): potential speed-up by *not* bootstrapping the units within
  # which the deviation is constant (e.g. units where all graders agree on the
  # same grade)
  # sampled.deviations.by_unit <- deviations[,
  #   .(sample = 1:nboot, deviations = bootstrapWithinUnit(deviation, nboot)),
  #   keyby="unit"]
  # =============================


  # Since we know for now taht we only have two graders, hence mu=2 for all
  # units, we can resample pairs accross units without stratifying.
  # TODO(jucor): implement bootstrap for more than two graders
  sampled_deviations <- deviations[
    sample.int(.N, .N * nboot, replace = TRUE),
    .(deviation, sample = seq(1, nboot))
  ]


  # TODO(jucor): for the case where mu is constant throughout all units (e.g.
  # for 2 observers), sample from the much much smaller observed coincidence
  # matrix rather than from all the pairs.

  # And compute the final alphas, summing over the deviations for each unit
  samples <- sampled_deviations[, .(alpha = 1 - sum(deviation)), by = "sample"]

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

.onUnload <- function(libpath) { # nolint
  library.dynam.unload("krippendorff", libpath)
}
