\name{frescalo}
\alias{frescalo}
\title{Frescalo}
\usage{
  frescalo(data = NULL, taxon_name = NULL,
    species_to_include = NULL, ignore.ireland = F,
    ignore.channelislands = F, sinkdir = NULL,
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
  hectads are removed. Default is \code{FALSE}}

  \item{ignore.channelislands}{Logical, if \code{TRUE}
  channel island hectads are removed. Default is
  \code{FALSE}}

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
  Results are saved to file and most are returned in a list
  to R.

  The list object returned is comprised of the following:

  \item{\bold{$paths}}{This list of file paths provides the
  locations of the raw data files for $log, $stat, $freq
  and $trend, in that order}

  \item{\bold{$trend}}{This dataframe provides the list of
  time factors for each species}

  \tabular{rll}{ - \tab \code{Species} \tab Name of
  species\cr - \tab \code{Time} \tab Time period, specified
  as a class (e.g. 1970); times need not be numeric and are
  indexed as character strings\cr - \tab \code{TFactor}
  \tab Time factor, the estimated relative frequency of
  species at the time\cr - \tab \code{St_Dev} \tab Standard
  deviation of the time factor, given that spt (defined
  below) is a weighted sum of binomial variates\cr - \tab
  \code{Count} \tab Number of occurrences of species at the
  time period\cr - \tab \code{spt} \tab Number of
  occurrences, given reduced weight of locations having
  very low sampling effort\cr - \tab \code{est} \tab
  Estimated number of occurrences; this should be equal to
  spt if the algorithm has converged\cr - \tab
  \code{N>0.00} \tab Number of locations with non-zero
  probability of the species occurring\cr - \tab
  \code{N>0.98} \tab Number of locations for which the
  probability of occurrence was estimated as greater than
  0.98\cr }

  \item{\bold{$stat}}{Location report}

  \tabular{rll}{ - \tab \code{Location} \tab Name of
  location; in this case locations are hectads of the GB
  National Grid \cr - \tab \code{Loc_no} \tab Numbering
  (added) of locations in alphanumeric order \cr - \tab
  \code{No_spp} \tab Number of species at that location;
  the actual number which may be zero \cr - \tab
  \code{Phi_in} \tab Initial value of phi, the
  frequency-weighted mean frequency \cr - \tab \code{Alpha}
  \tab Sampling effort multiplier (to achieve standard
  value of phi) \cr - \tab \code{Wgt_n2} \tab effective
  number N2 for the neighbourhood weights; this is small if
  there are few floristically similar hectads close to the
  target hectad.  It is (sum weights)^2 / (sum weights^2)
  \cr - \tab \code{Phi_out} \tab Value of phi after
  rescaling; constant, if the algorithm has converged\cr -
  \tab \code{Spnum_in} \tab Sum of neighbourhood
  frequencies before rescaling\cr - \tab \code{Spnum_out}
  \tab Estimated species richness, i.e. sum of
  neighbourhood frequencies after rescaling\cr - \tab
  \code{Iter} \tab Number of iterations for algorithm to
  converge\cr }

  \item{\bold{$freq}}{Listing of rescaled species
  frequencies}

  \tabular{rll}{ - \tab \code{Location} \tab Name of
  location\cr - \tab \code{Species} \tab Name of species\cr
  - \tab \code{Pres} \tab Record of species in location (1
  = recorded, 0 = not recorded)\cr - \tab \code{Freq} \tab
  Frequency of species in neighbourhood of location\cr -
  \tab \code{Freq_1} \tab Estimated probabilty of
  occurrence, i.e. frequency of species after rescaling\cr
  - \tab \code{SD_Frq1} \tab Standard error of Freq_1,
  calculated on the assumption that Freq is a binomial
  variate with standard error sqrt(Freq*(1-Freq)/ Wgt_n2),
  where Wgt_n2 is as defined for samples.txt in section
  (b)\cr - \tab \code{Rank} \tab Rank of frequency in
  neighbourhood of location\cr - \tab \code{Rank_1} \tab
  Rescaled rank, defined as Rank/Estimated species
  richness\cr }

  \item{\bold{$log}}{This records all the output sent to
  the console when running frescalo}

  \item{\bold{$lm_stats}}{The results of linear modelling
  of TFactors}

  \tabular{rll}{ - \tab \code{SPECIES} \tab Name of species
  used internally by frescalo\cr - \tab \code{NAME} \tab
  Name of species as appears in raw data\cr - \tab \code{b}
  \tab The slope of the model\cr - \tab \code{a} \tab The
  intercept\cr - \tab \code{b_std_err} \tab Standard error
  of the slope\cr - \tab \code{b_tval} \tab t-value for a
  test of significance of the slope\cr - \tab \code{b_pval}
  \tab p-value for a test of significance of the slope\cr -
  \tab \code{a_std_err} \tab Standard error of the
  intercept\cr - \tab \code{a_tval} \tab t-value for a test
  of significance of the intercept\cr - \tab \code{a_pval}
  \tab p-value for a test of significance of the
  intercept\cr - \tab \code{adj_r2} \tab Rescaled rank,
  defined as Rank/Estimated species richness\cr - \tab
  \code{r2} \tab t-value for a test of significance of the
  intercept\cr - \tab \code{F_val} \tab F-value of the
  model\cr - \tab \code{F_num_df} \tab Degrees of freedom
  of the model\cr - \tab \code{F_den_df} \tab Denominator
  degrees of freedom from the F-statistic \cr } \bold{The
  following columns are only produced when there are only
  two time periods} \tabular{rll}{ - \tab \code{Z_VAL} \tab
  Z-value for the significance test of the trend\cr - \tab
  \code{SIG_95} \tab A logical statement indicating if the
  trend is significant (TRUE) or non-significant (FALSE)\cr
  }
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

