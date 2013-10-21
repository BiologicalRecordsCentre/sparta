\name{reformat_gr}
\alias{reformat_gr}
\title{Reformat OSGB grid references}
\usage{
  reformat_gr(gridref, prec_out = 10000, precision = NULL)
}
\arguments{
  \item{gridref}{A string, or vector of strings, giving the
  gidreference to be transformed}

  \item{prec_out}{The precision of the returned
  gridreference in meters.}

  \item{precision}{The precision of the original grid
  reference in meters, if not given then the function
  attempts to calculate the precision.}
}
\value{
  A string, or vector of strings.
}
\description{
  Converts grid references from high precision to lower
  precision. for example giving the 10km square that a 1km
  square is found in.
}
\examples{
\dontrun{

 # Load the library
 library(sparta)

 reformat_gr('SP508448', prec_out = 10000)

}
}
\keyword{grid}
\keyword{OSGB}
\keyword{reference}
\keyword{UK}

