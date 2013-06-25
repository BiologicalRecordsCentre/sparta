\name{telfer}
\alias{telfer}
\title{Telfer's change index}
\usage{
  telfer(Data = NULL, time_periods = NULL,
    ignore.ireland = F, ignore.channelislands = F,
    sinkdir = NULL, min_sq = 5, useIterations = TRUE,
    iterations = 10, year_col = NA, site_col = NA,
    sp_col = NA, start_col = NA, end_col = NA)
}
\arguments{
  \item{Data}{The data to be analysed. This should consist
  of rows of observations and columns indicating the
  species and location as well as either the year of the
  observation or columns specifying the start and end dates
  of the observation. This can be a dataframe object or a
  string giving the file path to a .csv or .rdata file. If
  left blank you will be prompted for a .csv or .rdata
  file.}

  \item{time_periods}{This specifies the time periods to be
  analysed. A dataframe object with two columns. The first
  column contains the start year of each time period and
  the second column contains the end year of each time
  period.  Time periods should not overlap.}

  \item{ignore.ireland}{If \code{TRUE} data from hectads in
  Ireland are removed.}

  \item{ignore.channelislands}{If \code{TRUE} data from
  hectads in the Channel Islands are removed.}

  \item{sinkdir}{An optional argument giving a file path
  where results should be written.  This is useful if
  running the function in a loop over a number of datasets.
  Results are still returned to R when using
  \code{sinkdir}.}

  \item{min_sq}{The minimum number of squares occupied in
  the first time period in order for a trend to be
  calculated for a species.}

  \item{useIterations}{A logical variable indicating
  whether iterations should be used.  Iterations are used
  to account for increased variation in the logit
  proportions close to zero and one (see Telfer et al
  2002). Default is \code{TRUE}}

  \item{iterations}{If \code{useIterations} is \code{TRUE},
  then this parameter indicates the number of iterations to
  be used. In Telfer et al 2002 the number of iterations
  used were 7 and 8 for the two datasets for which it was
  applied. The defualt here is 10.}

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
  A dataframe of results are returned to R. The first
  column gives the names of each species. Each subsequent
  column gives the result of a pairwise comparison between
  two time periods. The numbers in column headings indicate
  the time periods compared i.e. '1_2' indicates a
  comparison of the 1st and 2nd time periods '3_5'
  indicates a comparison of the 3rd and 5th time period.
  Time periods are ordered by their start year.
}
\description{
  Telfers change index is designed to assess the relative
  change in range size of species between two time periods
  (Telfer et al 2002). This function can take multiple time
  periods and will complete all pairwise comparisons.
}
\examples{
\dontrun{

 # Load example dataset
 data(ex_dat)

 # Passing data as an R object
 telfer_out <- plrtelfer(Data=ex_dat,
                         time_periods=data.frame(start=c(1980,1990,2000),end=c(1989,1999,2009)),
                         min_sq=2,
                         useIterations=T,
                         iterations=20,
                         site_col='hectad',
                         sp_col='CONCEPT',
                         start_col='TO_STARTDATE',
                         end_col='Date')

}
}
\references{
  Telfer, M.G., Preston, C.D., & Rothery, P. (2002) A
  general method for measuring relative change in range
  size from biological atlas data.  Biological
  Conservation, 107, 99-109.
}
\keyword{species}
\keyword{telfer,}
\keyword{trends,}

