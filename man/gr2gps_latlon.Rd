\name{gr2gps_latlon}
\alias{gr2gps_latlon}
\title{Covert grid reference to latitude and longitude}
\usage{
  gr2gps_latlon(gridref, precision = NULL,
    projection = "OSGB", centre = TRUE)
}
\arguments{
  \item{gridref}{a vector of strings giving grid references
  to be changed}

  \item{precision}{gives the precision of the grid
  references. Must be of the same length as gridref.
  precision is given in meters as a numeric i.e. 10km
  square = 10000, 1km square = 1000. If NULL then the
  function tries to work out the precision itself.}

  \item{projection}{specifies the projection of the input
  and can either be \code{"OSGB"} or \code{"OSNI"}.
  Defaults to \code{"OSGB"}}

  \item{centre}{If \code{TRUE} the coordinates for the
  centre of the cell are given else if \code{FALSE} the
  coordinates for the bottom left corner are given.}
}
\value{
  A dataframe of results are returned
}
\description{
  Covert grid reference to latitude and longitude
}
\examples{
gr2gps_latlon('SU616896')
}
\keyword{grid-reference}
\keyword{latitude}
\keyword{longitude}
\keyword{mapping}

