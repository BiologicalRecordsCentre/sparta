\name{parallelMixedModel}
\alias{parallelMixedModel}
\title{Parallelised Mixed-model trend analysis}
\usage{
  parallelMixedModel(Data = NULL, year_range = NULL,
    ignore.ireland = F, ignore.channelislands = F,
    min_list = 2, min_years = 3, od = FALSE,
    verbose = FALSE, sinkdir = NULL,
    trend_option = "arithmetic", NYears = 10,
    date_col = NA, site_col = NA, sp_col = NA,
    start_col = NA, end_col = NA, print_progress = TRUE,
    cores = 1)
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

  \item{min_list}{The minimum list length (number of
  species) required from a visit for it to be included in
  the mixed model analysis. Default is 2 (as in Roy et al,
  2010) but should be changed dependent on the distribution
  of list lengths in the data.}

  \item{min_years}{This variable defines the minimum number
  of years in which a site must have well sampled visits
  (defined by \code{min_list}) to be included in te
  analysis. The default is set to 3 as in Roy et al
  (2010).}

  \item{od}{This option allows modelling overdispersion
  (\code{TRUE}) in mixed models.  Default is \code{FALSE}.}

  \item{verbose}{This option, if \code{TRUE}, sets mixed
  model verbose allowing the interations of each model to
  be veiwed.}

  \item{sinkdir}{An optional argument specifying the file
  path where output should be saved.  If the folder does
  not exist it will be created. Files are titled as a
  concatenation of 'Mixed-Models_' and the date in yymmdd
  format (i.e. 'Mixed-Models_130702.csv').  If a file of
  this name already exists in the directory specified the
  file name will be appended with an index number (i.e.
  'Mixed-Models_130702(1).csv').}

  \item{trend_option}{Set the method by which you wish to
  calculate percentage change. This can currently be set to
  either \code{'arithmetic'} (default) or
  \code{'geometric'}. Arimthmetic calculates percentage
  change in a linear fashion such that a decline of 50\%
  over 50 years is equal to 10\% in 10 years. Using the
  same example a Geometric trend would be 8.44\% every 10
  years as this work on a compound rate.}

  \item{NYears}{The number of years over which you want the
  percentage change to be calculated (i.e.  10 gives a
  decadal change). Default = 10}

  \item{date_col}{The name of the date column in
  \code{Data}. This column must be a date and should be in
  date format. If it isn't, a conversion is attempted. This
  column defines the temporal period of a visit.}

  \item{site_col}{The name of the site column in
  \code{Data}. This column defines the spatial scale of a
  visit and so could be a unique site name (e.g.
  \code{'West Farm'}), or larger scale spatial unit (e.g. a
  1km square).}

  \item{sp_col}{The name of the species column in
  \code{Data}}

  \item{start_col}{The name of the start date column in
  \code{Data}. This must be a date and should be in date
  format. If it isn't a conversion is attempted. Note that
  rows where start_col and end_col are different will be
  removed from the analysis. Where rows are removed due to
  differences in values between start_col and end_col a
  warning is given.}

  \item{end_col}{The name of the end date column in
  \code{Data}. See \code{start_col}.}

  \item{print_progress}{Logical, if \code{TRUE} progress is
  printed to console when running models. Default is
  \code{TRUE} cores The number of cores to be used in
  parallel processing. Defaults to 1 (not parallel)}
}
\value{
  A dataframe of results are returned to R. Each row gives
  the results for a single species, with the species name
  given in the first column. The columns \code{year} and
  \code{intercept} give the estimates of these
  coefficients.  The suffix \code{SE} indicates the
  standard error, \code{zscore} the Z-score and
  \code{pvalue} the P-value. \code{yearZero} gives the year
  used for the intercept (i.e. that year whose value is set
  to 0). This helps to increase model stability.
  \code{cvg_code} gives the convergence code for each of
  the models. A value of 4 indicates a good model, other
  values can be looked up using cvg_codes().
  \code{pVisitsUsed} gives the proportion of all visits
  (unique combinations of date and site) used in the
  modelling after removing short lists (\code{min_list}),
  and sites sampled in few years (\code{min_years}).
  \code{nVisitsUsed} is similar, giving the absolute number
  of visits used, and \code{nSpeciesObs} gives the number
  of these visits when the species of interest was
  observed.  \code{Ymin} and \code{Ymax} give the minimum
  and maximum years used in the model. Note these values
  are centered on \code{yearZero}.  \code{change_} gives
  the percentage change dependent on the values given to
  \code{trend_option} and \code{NYears}.\cr \cr If
  \code{sinkdir} is given them a log file is created in the
  same directory.  This can be used to check on progress of
  the analysis since when using parallel print statements
  are not returned to the console.
}
\description{
  This function is identical in functio to
  \code{mixedModel} except that it runs the analysis using
  the package \code{parallel}. The only difference is that
  the user must specify the number of cores to be used.
  Additionally since warning messages from slaves are
  suppressed and the order of species in output may
  vary.\cr This function undertakes a 'mixed-model'
  analysis as laid out by Roy et al (2012). This method
  accounts for variation in recording intensity between
  sites and excludes data that may introduce error into
  trend estimates.
}
\examples{
\dontrun{

library(sparta)

#load example dataset
data(ex_dat)

MM_out<-mixedModel(Data=ex_dat,
                   year_range=c(1970,2000),
                   min_list=1,
                   min_years=2,
                   site_col='kmsq',
                   sp_col='CONCEPT',
                   start_col='TO_STARTDATE',
                   end_col='Date',
                   cores=2)

}
}
\references{
  Roy, H.E., Adriaens, T., Isaac, N.J.B. et al. (2012)
  Invasive alien predator causes rapid declines of native
  European ladybirds. Diversity & Distributions, 18,
  717-725.
}
\keyword{distribution}
\keyword{species,}
\keyword{trends,}

