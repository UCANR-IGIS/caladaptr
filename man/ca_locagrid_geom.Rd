% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ca_locagrid_geom.R
\name{ca_locagrid_geom}
\alias{ca_locagrid_geom}
\title{Get the LOCA grid cells as a sf object}
\usage{
ca_locagrid_geom(quiet = FALSE)
}
\arguments{
\item{quiet}{Suppress messages}
}
\value{
A simple feature data frame
}
\description{
Get the geometry of the LOCA grid cells as a sf polygon object
}
\details{
This retrieves the geometry of the LOCA grid as a vector (polygon)
layer. The cells in this grid represent the pixels for all the LOCA downscaled
raster series on Cal-Adapt. A copy of the layer will be saved in the cache folder
so it won't have to be downloaded more than once.
}
\seealso{
\code{\link{ca_getcache}}
}
