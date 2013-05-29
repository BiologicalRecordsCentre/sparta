\name{create_weights}
\alias{create_weights}
\title{create frescalo weights file}
\usage{
  create_weights(dist = NULL, sim = NULL, dist_sub = 200,
    sim_sub = 100, normalise = FALSE)
}
\arguments{
  \item{dist}{a dataframe giving the distance between sites
  in a long format with three columns. The first column
  give the ID of the first site, the second column gives
  the ID of the second site and the third column gives the
  distance between them.  The table should include
  reciprocal data i.e. both rows 'A, B, 10' and 'B, A, 10'
  should exist.}

  \item{sim}{a dataframe of numeric attributes of each
  site. The first column must contain the site IDs and all
  other columns are used to calculate the similarity
  between sites using dist() and method 'euclidean'.}

  \item{dist_sub}{the number of neighbours to include after
  ranking by distance. In Hill (2011), this is set to 200
  and is the default here}

  \item{sim_sub}{the number of neighbours to include after
  ranking by similarity. This is the final number of site
  that will be included in a neighbourhood. In Hill (2011),
  this is set to 100 and is the default here.}

  \item{normalise}{Logical. If \code{TRUE} each attribute
  is divided by its maximum value to produce vlues between
  0 and 1. Default is \code{FALSE}}
}
\value{
  A dataframe is returned in a format that can be used
  directly in frescalo() or sparta(). The dataframe has
  three columns giving the target cell, the neighbourhood
  cell, and the weight.
}
\description{
  This file creates the weights file required to run
  frescalo, as outlined in (Hill, 2011). For more
  information on frescalo see \code{\link{frescalo}}. This
  function takes a table of geographical distances between
  sites and a table of numeric data from which to calculate
  similarity (for example, landcover or abiotic data)
}
\examples{
\dontrun{
library(sparta)

# Load example distance and similarity data
data(sim)
data(dist)

# Create the weights file
weights<-create_weights(dist=dist,
                       sim=sim,
                       dist_sub=20,
                       sim_sub=10)

# Set up sinkdir to run frescalo with our new weights file
sinkdir <- getwd()

# Load example data for frescalo
data(ex_dat)

# Run frescalo
  fres_out <- frescalo(data = ex_dat,
                    taxon_name = 'Unicorns',
                    time_periods = data.frame(start=c(1980,1990),end=c(1989,1999)),
                    sinkdir = sinkdir,
                    Fres_weights=weights)
 }
}
\references{
  Hill, Mark. Local frequency as a key to interpreting
  species occurrence data when recording effort is not
  known. 2011. \emph{Methods in Ecology and Evolution}, 3
  (1), 195-205.
}
\keyword{frescalo,}
\keyword{trends,}
\keyword{weights}

