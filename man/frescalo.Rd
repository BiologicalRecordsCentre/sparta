\name{frescalo}
\alias{frescalo}
\title{Frescalo}
\usage{
  frescalo(data = NULL, taxon_name = NULL,
    species_to_include = NULL, ignore.ireland = T,
    ignore.channelislands = T, sinkdir = NULL,
    taxon_reg = NULL, time_periods = NULL, channel = NULL,
    Plot_Fres = TRUE, Fres_weights = "LC",
    non_benchmark_sp = NULL, get_names_from_BRC = FALSE,
    fres_site_filter = NULL)
}
\arguments{
  \item{data}{A dataframe object or string giving the file
  path to the location of data (either .rdata or .csv).
  Required columns are: 'CONCEPT', 'hectad' and 'year'.
  Optionally 'TO_STARTDATE' can be included, if it is this
  is assumed to be the start date (from which start year is
  extracted) and year is assumed to be the end date. If
  \code{NULL} the user is prompted to select a .csv or
  .rdata file.}

  \item{taxon_name}{string giving the name of data. This is
  used to name output files when written}

  \item{species_to_include}{A vector of strings (that match
  your CONCEPT column in data) which are to be used.
  Species not in your list are ignored.  This is useful if
  you are only interested in a subset of species, i.e. in
  red listing}

  \item{ignore.ireland}{Logical, if \code{TRUE} Irish
  hectads are removed. Default is \code{TRUE}}

  \item{ignore.channelislands}{Logical, if \code{TRUE}
  channel island hectads are removed. Default is
  \code{TRUE}}

  \item{sinkdir}{String giving the output directory for
  result}

  \item{get_names_from_BRC}{Default is \code{FALSE}. if
  \code{TRUE} it assumes the CONCEPT in your data relates
  to a BRC concept code and the names of species will be
  retireve from the database using \code{channel} for use
  in output.}

  \item{time_periods}{A dataframe object with two columns.
  The first column contains the start year of each time
  period and the second column contains the end year of
  each time period. This is required if running Frescalo or
  basic trends.}

  \item{channel}{An ODBC channel, creaded using
  odbcConnect(), this can be used to get spp names if
  get_names_from_BRC is \code{TRUE}}

  \item{Plot_Fres}{Logical, if \code{TRUE} maps are
  produced by Frescalo}

  \item{Fres_weights}{'LC' specifies a weights file based
  on landcover data for the UK and 'VP' uses a weights file
  based on vascular plant data for the UK , both are
  included in the package. Alternativly a custom weights
  file can be given as a data.frame. This must have three
  columns: target cell, neighbour cell, weight.}

  \item{non_benchmark_sp}{a character vector giving the
  concepts of species not to be used as benchmarks in
  Frescalo}

  \item{fres_site_filter}{Optionally a character vector of
  the names of sites to be used for in the trend analysis.
  Sites not include in this list are not used for
  estimating TFactors. Default is \code{NULL} and all sites
  are used}
}
\value{
  Results are saved to file and the relevant files are
  returned in a list
}
\description{
  A function for using Frescalo (Hill, 2011), a tool for
  analysing occurrence data when recording effort is not
  known. This function returns the output from Frescalo to
  the R session and saves it to the path specified by
  \code{sinkdir}. By setting \code{Plot_Fres} to
  \code{TRUE} maps of the results will also be saved.
  Plotting the returned object gives a useful summary.
}
\examples{
\dontrun{
#script for testing frescalo
#data will be written to your working directory

data(ex_dat)

x<-frescalo(data=ex_dat,
         taxon_name='EXAMPLE',
         time_periods=data.frame(start=c(1980,1990),end=c(1989,1999)),
         sinkdir=paste(getwd(),'/example_sparta_output',sep=''))
}
}
\references{
  Hill, Mark. Local frequency as a key to interpreting
  species occurrence data when recording effort is not
  known. 2011. \emph{Methods in Ecology and Evolution}, 3
  (1), 195-205.
}
\keyword{frescalo}
\keyword{trends,}

