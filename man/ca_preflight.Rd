% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ca_preflight.R
\name{ca_preflight}
\alias{ca_preflight}
\title{Run checks on an API request object}
\usage{
ca_preflight(
  x,
  slug_check = TRUE,
  date_check = TRUE,
  loc_check = TRUE,
  units_check = TRUE,
  spag_check = TRUE,
  check_for = c("getvals", "getrst"),
  quiet = FALSE,
  ignore_spag = deprecated()
)
}
\arguments{
\item{x}{A Cal-Adapt API request}

\item{slug_check}{Cross check the slug against the raster series catalog}

\item{date_check}{Cross check the start and end date against the raster series catalog}

\item{loc_check}{Check to make sure the location is within the Cal-Adapt coverage area}

\item{units_check}{Check for consistent units}

\item{spag_check}{Check spatial aggregation option}

\item{check_for}{What to check for - getting values or getting rasters}

\item{quiet}{Suppress messages}

\item{ignore_spag}{Deprecated}
}
\value{
TRUE if no messages are reported, else FALSE
}
\description{
Run checks on an API request object
}
\details{
This function checks an Cal-Adapt API request for potential problems. It checks to make sure
the request:

\itemize{
   \item is complete
   \item doesn't have conflicting elements
   \item specifies an existing datasets
   \item specifies a location within the Cal-Adapt coverage area
   \item specifies area-of-interest presets correctly
   \item has features that are not too large for the Cal-Adapt API
   \item doesn't mix datasets that have different units
   \item uses dates that fall within the Cal-Adapt time series
   \item includes a spatial aggregation function if needed
}

Most of the checks can be selectively disabled using arguments. \code{check_for} allows you to tailor checks for
querying values and/or downloading rasters.
}
\seealso{
\code{\link{ca_catalog_rs}}
}
