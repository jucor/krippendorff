% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{clean_and_count}
\alias{clean_and_count}
\title{Prepare and clean a data.table for quadrilogy}
\usage{
clean_and_count(dt, unit_from, measurement_from, frequency_from)
}
\arguments{
\item{dt}{`data.table` containing measurements, either coders or standard.}

\item{unit_from}{Name of the column containing the unit ID}

\item{measurement_from}{Name of the column containing the measurements}

\item{frequency_from}{(Optional) Name of the column containing the
frequencies, *if* the data is in the "aggregated".}
}
\value{

}
\description{
Standardizes the names of the columns, set the keys,
compute the frequencies
}
