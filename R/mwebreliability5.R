#' Example data from Krippendorff 2013
#'
#' Example reliablility data, with multiple observers and missing data,
#' from Krippendorf (2013, page 4), 4 observers, 12 units. This example can be
#' interpreted either as nominal, ordinal, or interval data
#'
#' It is cast to_long_form,
#' with missing observations removed from the dataset.
#' See \code{\link{mwebreliability5.wide}} for the wide version ontaining NA.
#'
#' TODO(jucor): Find how to cite Hayes 2013 properly with @references
#'
#' @seealso \code{\link{mwebreliability5.wide}}
#' @format A `data.table` with 41 rows and 3 variables:
#' \describe{
#' \item{unit}{Unit observed}
#' \item{observer}{Name of the observer}
#' \item{measurement}{Measurement from the observer, between 1 and 4}
#' }
"mwebreliability5"
