\name{maes}
\alias{maes}
\title{Maes trend analysis}
\usage{
  maes(Data = NULL, time_periods = NULL,
    ignore.ireland = F, ignore.channelislands = F,
    sinkdir = NULL, min_sp = 5, year_col = NA,
    site_col = NA, sp_col = NA, start_col = NA,
    end_col = NA)
}
\arguments{
  \item{Data}{A dataframe object or string giving the file
  path to the location of data (either .rdata or .csv).
  This should consist of rows of observations and columns
  indicating the species and location as well as either the
  date/year of the observation or columns specifying the
  start and end dates/years of the observation. If
  \code{NULL} (default) the user is prompted to select a
  .csv or .rdata file.}

  \item{time_periods}{This parameter gives the time periods
  to be compared. A dataframe object with two columns. The
  first column contains the start year of each time period
  and the second column contains the end year of each time
  period. Time periods should not overlap.}

  \item{ignore.ireland}{If \code{TRUE} data from hectads in
  Ireland are removed.}

  \item{ignore.channelislands}{If \code{TRUE} data from
  hectads in the Channel Islands}

  \item{sinkdir}{An optional argument specifying the file
  path where output should be saved.  If the folder does
  not exist it will be created. Files are titled as a
  concatenation of 'Maes_' and the date in yymmdd format
  (i.e. 'Maes_130702.csv'). If a file of this name already
  exists in the directory specified the file name will be
  appended with an index number (i.e.
  'Maes_130702(1).csv').}

  \item{min_sp}{A numeric argument giving the minimum
  number of species that must be observed in both time
  periods for a site to be included. For example, if set to
  5, then at least 5 species much be observed in both time
  period 1 and time period 2 for that site to be included
  in the analysis. Default is 5, as in Maes et al, (2012).}

  \item{year_col}{The name of the year column in
  \code{Data}}

  \item{site_col}{The name of the site column in
  \code{Data}}

  \item{sp_col}{The name of the species column in
  \code{Data}}

  \item{start_col}{The name of the start date column in
  \code{Data}}

  \item{end_col}{The name of the end date column in
  \code{Data}}
}
\value{
  A dataframe of results are returned to R. Each row gives
  the results for a single species, with the species name
  given in the first column. Each column name is prefixed
  with the time periods it addresses (this makes the
  results clear when many timeperiods are being compared).
  \code{1_2} indicates a comparison of time period 1 and 2.
  \code{gridcells1} and \code{gridecells2} give the number
  of cells occupied in each time period (1st and 2nd
  respectively) after removing sites that do not meet the
  selection criteria. \code{relDist1} and \code{relDist2}
  give the relative distribution, and \code{change} gives
  the percentage change.
}
\description{
  This function uses the method presented in Maes et al,
  (2012) to estimate a percentage change in distribution
  area between two time periods. The formula attempts to
  account for uneven recording effort between time periods.
  This method is specifically designed to feed into IUCN
  red list analyses (criterion A2c). This function can take
  multiple time periods and will complete all pairwise
  comparisons
}
\examples{
\dontrun{

 # Load the library
 library(sparta)

 # Load example dataset
 data(ex_dat)

 # Run the analysis
 Ma_out<-maes(Data=ex_dat,
              time_periods=(data.frame(start=c(1980,1990,2000),end=c(1989,1999,2009))),
              min_sp=1,
              site_col='hectad',
              sp_col='CONCEPT',
              start_col='TO_STARTDATE',
              end_col='Date')

}
}
\references{
  Maes, D., Vanreusel, T., Jacobs, I., Berwaerts, K., Van
  Dyck, H. (2012) Applying IUCN Red List criteria at a
  small regional level: a test case with butterflies in
  Flanders (north Belgium). Biological Conservation, 145,
  258-266.
}
\keyword{distribution,}
\keyword{IUCN,}
\keyword{list,}
\keyword{Maes}
\keyword{red}
\keyword{species,}
\keyword{trends,}

