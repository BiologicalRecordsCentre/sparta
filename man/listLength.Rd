\name{listLength}
\alias{listLength}
\title{List-length trend analysis}
\usage{
  listLength(Data = NULL, year_range = NULL,
    ignore.ireland = F, ignore.channelislands = F,
    visit_scale = "date", sinkdir = NULL,
    trend_option = "arithmetic", NYears = 10,
    date_col = NA, site_col = NA, sp_col = NA,
    start_col = NA, end_col = NA, print_progress = TRUE)
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

  \item{year_range}{A vector of two numbers, the start and
  end year of the time period you wish to analyse. This
  simply subsets your data to the given year range.}

  \item{ignore.ireland}{If \code{TRUE} data from hectads in
  Ireland are removed.}

  \item{ignore.channelislands}{If \code{TRUE} data from
  hectads in the Channel Islands are removed.}

  \item{visit_scale}{A recording 'list' can be defined as
  the species observed in a single outing (i.e. unique
  combination of site and date). However, one may also
  choose to define a list as the unique combination of site
  and year. If \code{visit_scale} is \code{'date'} then a
  visit is defined as a combination of date and site. If
  \code{visit_scale} is \code{'year'} then a visit is
  defined as a unique combination of year and site. Default
  is \code{'date'}.}

  \item{sinkdir}{An optional argument specifying the file
  path where output should be saved.  If the folder does
  not exist it will be created. Files are titled as a
  concatenation of 'List-length_Models_' and the date in
  yymmdd format (i.e. 'List-length_Models_130702.csv').  If
  a file of this name already exists in the directory
  specified the file name will be appended with an index
  number (i.e. 'List-length_Models_130702(1).csv').}

  \item{date_col}{The name of the date column in
  \code{Data}. When using \code{visit_scale='date'} this
  column must be a date and should be in date format. If it
  isn't a conversion is attempted.  If using
  \code{visit_scale='year'} then date_col can be a numeric
  representation of year (i.e. 1998) or a date (dates will
  be converted to years for the analysis). Again, if the
  date is not in date format conversion is attempted.}

  \item{site_col}{The name of the site column in
  \code{Data}. This column defines the spatial scale of a
  visit and so could be a unique site name (e.g.
  \code{'West Farm'}), or larger scale spatial unit (e.g. a
  1km square).}

  \item{trend_option}{Set the method by which you wish to
  calculate %change. This can currently be set to either
  \code{'arithmetic'} (default) or \code{'geometric'}.
  Arimthmetic calculates percentage change in a linear
  fashion such that a decline of 50% over 50 years is equal
  to 10% in 10 years. Using the same example a Geometric
  trend would be 8.44% every 10 years as this work on a
  compound rate.}

  \item{NYears}{The number of years over which you want the
  %change to be calculated (i.e.  10 gives a decadal
  change). Default = 10}

  \item{sp_col}{The name of the species column in
  \code{Data}}

  \item{start_col}{The name of the start date column in
  \code{Data}. When using \code{visit_scale='date'} this
  must be a date and should be in date format. If it isn't
  a conversion is attempted.  Note that rows where
  start_col and end_col are different will be removed from
  the analysis.  If using \code{visit_scale='year'} then
  start_col can be a numeric representation of year (i.e.
  1998) or a date (dates will be converted to years for the
  analysis). Again, if the date is not in date format
  conversion is attempted, and rows where start_col and
  end_col are different will be removed. Where rows are
  removed due to differences in values between start_col
  and end_col a warning is given.}

  \item{end_col}{The name of the end date column in
  \code{Data}. See \code{start_col}.}

  \item{print_progress}{Logical, if \code{TRUE} progress is
  printed to console when running models. Default is
  \code{TRUE}}
}
\value{
  A dataframe of results are returned to R. Each row gives
  the results for a single species, with the species name
  given in the first column. The columns \code{year},
  \code{intercept} and \code{Log2LL} give the estimates of
  the year coefficient, intercept, and coefficeint of log
  to base 2 list-length respectively For each of these
  estimates the standard error (\code{SE}), Z-score
  (\code{zscore}) and P-value (\code{pvalue}) are given.
  The year used for the intercept is year 0 but the median
  year in the dataset, this helps to increase model
  stability and is given in column \code{yearZero}.
  \code{Ymin} and \code{Ymax} give the minimum and maximum
  years used in the model. Note these values are centered
  on \code{yearZero}. \code{change_} gives the percentage
  change dependent on the values given to
  \code{trend_option} and \code{NYears}.
}
\description{
  This function undertakes a list-length analysis (Szabo et
  al, 2010) providing estimated trends in species
  occurrence data, accounting for the length of recording
  lists. The method is based on a maximum likelihood
  implementation of the analysis shown in Szabo et al
  (2010).
}
\examples{
\dontrun{

library(sparta)

#load example dataset
data(ex_dat)

LL_out <- listLength(Data = ex_dat,
                     year_range = c(1970,2010),
                     site_col = 'kmsq',
                     sp_col = 'CONCEPT',
                     start_col='TO_STARTDATE',
                     end_col='Date')


}
}
\references{
  Szabo, J.K., Vesk, P.A., Baxter, P.W.J., & Possingham,
  H.P. (2010) Regional avian species declines estimated
  from volunteer-collected long-term data using List Length
  Analysis. Ecological Applications, 20, 2157-2169.
}
\keyword{distribution,}
\keyword{length,}
\keyword{list-length}
\keyword{list,}
\keyword{species,}
\keyword{trends,}

