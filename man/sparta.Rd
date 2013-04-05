\name{sparta}
\alias{sparta}
\title{Trend analyses for unstructured data}
\usage{
  sparta(data = NULL, taxon_name = NULL, Run_models = T,
    Run_MM = T, Run_LL = T, Run_Fres = T, Run_Basic = T,
    Create_persistance_table = F,
    Create_persistance_summary = F,
    Calc_D_england_only = F, Year.range = NULL,
    res = "visit", min.L = 4, min.yrs = 3,
    wellsamp = "visit", split_yr = NULL,
    species_to_include = NULL, ignore.ireland = F,
    ignore.channelislands = F, sinkdir = NULL, Log = T,
    taxon_reg = NULL, print_progress = T,
    time_periods = NULL, channel = NULL, Plot_Fres = TRUE,
    Fres_weights = "LC", non_benchmark_sp = NULL,
    Telfer_min_sq = 5, get_names_from_BRC = FALSE,
    fres_site_filter = NULL)
}
\arguments{
  \item{data}{A dataframe object or a vector of strings
  giving the file path to the location(s) of data this can
  be in either .rdata or .csv format.  For Frescalo
  required columns are: 'CONCEPT', 'hectad' and 'year'.
  Optionally 'TO_STARTDATE' can be included, if it is this
  is assumed to be the start date (from which start year is
  extracted) and year is assumed to be the end date.  If
  \code{NULL} the user is prompted to select a .csv or
  .rdata file.  For mixed models and list length models the
  following columns are required: 'CONCEPT', 'kmsq' and
  'Date', optionally 'TO_STARTDATE' can be added. For basic
  methods the following columns are required:
  'CONCEPT','hectad' and 'Date', optionally 'TO_STARTDATE'
  can be added.}

  \item{taxon_name}{A vector of strings giving the name(s)
  of data. This is used to name output files when written}

  \item{Run_models}{Logical, if \code{FALSE} no models are
  run. This overrules \code{Run_MM, Run_LL, Run_Fres} and
  \code{Run_Basic}. Default is \code{TRUE}}

  \item{Run_MM}{Logical, if \code{TRUE} mixed model
  analyses are run. Default is \code{TRUE}}

  \item{Run_LL}{Logical, if \code{TRUE} list-length model
  analyses are run. Default is \code{TRUE}}

  \item{Run_Fres}{Logical, if \code{TRUE} Frescalo analyses
  are run. Default is \code{TRUE}}

  \item{Run_Basic}{Logical, if \code{TRUE} mixed model
  analyses are run. Default is \code{TRUE}}

  \item{Create_persistance_table}{NOT IMPLEMENTED}

  \item{Create_persistance_summary}{NOT IMPLEMENTED}

  \item{Calc_D_england_only}{NOT IMPLEMENTED}

  \item{Year.range}{The time period over which you wish to
  analyse your trends. This may or may not be the same as
  min(time_periods):max(time_periods). This is used for
  analyses that do not use time periods, such as the mixed
  models and list length models}

  \item{res}{The 'resolution' at which mixed models and
  list-length models analyse the data. \code{'visit'}
  defines visits as unique combinations of km-square and
  date while \code{kmyr} defines visits as a unique
  combination of year km-square and year.}

  \item{min.L}{The minimum list length (number of species)
  required from a visit for it to be included in the mixed
  model analysis. Default is 4 but should probably be
  changed dependent on the dataset the analysis is being
  applied to}

  \item{min.yrs}{when \code{wellsamp} is \code{'visit'}
  then only sites for which there \code{min.yrs} number of
  well.sampled visits (as defined by \code{res} and
  \code{min.L}) are included in the mixed model analysis.
  If \code{wellsamp} is \code{'year'} then only sites with
  well sampled visits in \code{min.yrs} number of years are
  included in mixed model analysis.}

  \item{wellsamp}{defines what makes a site 'well sampled'
  and therefore included in the mixed model analysis. If
  \code{'visit'} then the site must have atleast
  \code{min.yrs} number of well.sampled visits (as defined
  by \code{res} and \code{min.L}). If \code{wellsamp} is
  \code{'year'} then only sites with well sampled visits in
  \code{min.yrs} number of years are included in mixed
  model analysis.}

  \item{split_yr}{USED IN PERSISTANCE TABLES, NOT
  IMPLEMENTED}

  \item{species_to_include}{A list of vector of strings
  (that match your CONCEPT column in data data) which are
  to be used. Species not in your list are ignored.  This
  is useful if you are only interested in a subset of
  species, i.e. in red listing}

  \item{ignore.ireland}{Logical, if \code{TRUE} Irish
  hectads are removed. Default is \code{FALSE}}

  \item{ignore.channelislands}{Logical, if \code{TRUE}
  channel island hectads are removed. Default is
  \code{FALSE}}

  \item{sinkdir}{String giving the output directory for
  result}

  \item{Log}{Logical, if \code{TRUE} log files are created
  in \code{sinkdir}}

  \item{taxon_reg}{A lookup table that gives names to your
  concepts. If not given the names are gathered from the
  taxon_register on Oracle (where possible)}

  \item{print_progress}{Logical, if \code{TRUE} progress is
  printed to console when running mixed models and list
  length models. Default is \code{TRUE}}

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

  \item{Telfer_min_sq}{The minimum number of squares
  occupied in the first time period in order for basic
  trends to be calculated for a species}

  \item{non_benchmark_sp}{a character vector giving the
  concepts of species not to be used as benchmarks in
  Frescalo}

  \item{get_names_from_BRC}{Default is \code{FALSE}. if
  \code{TRUE} it assumes the CONCEPT in your data relates
  to a BRC concept code and the names of species will be
  retireve from the database using \code{channel} for use
  in output.}

  \item{fres_site_filter}{Optionally a character vector of
  the names of sites to be used for in the trend analysis.
  Sites not include in this list are not used for
  estimating TFactors. Default is \code{NULL} and all sites
  are used}
}
\value{
  Results are save to file and most are returned to R.

  A list is returned:

  \item{\bold{$basic_methods}}{This gives the results of
  the basic methods. This includes three measures: power
  law residual (plr), Telfer's change index and
  proportional difference. These methods can only compare
  two time periods and so if there are more than two time
  periods in 'time_periods' these metrics are calculated
  for all pairwise comparisons. The number after the column
  name gives the time periods being compared where 1 is the
  first time period, 2 the second etc.}
  \item{\bold{$model_methods}}{This gives the results from
  the mixed models and list length models. [TOM TO ADD MORE
  DETAILS HERE]} \item{\bold{$frescalo}}{This gives the
  results of the frescalo analysis. See
  \code{\link{frescalo}}}
}
\description{
  A function that implements a range of methods used to
  analyses trends in unstructured occurrence datasets. If
  only running \code{\link{frescalo}}, then that function
  is recommended. This function additionally includes basic
  metrics such as Telfer and mixed-model and list-length
  models. In the future this function is likely to be
  replaced with a set of functions, one for each method
  since in its current state it is rather cumbersome.
}
\examples{
\dontrun{
#script for testing sparta
#data will be written to your working directory

data(ex_dat)

x<-sparta(data=ex_dat,
         taxon_name='EXAMPLE',
         time_periods=data.frame(start=c(1980,1990),end=c(1989,1999)),
         sinkdir=paste(getwd(),'/example_sparta_output',sep=''),
         Run_Fres=T,Run_MM=T,Run_LL=T,Log=T,Run_Basic=T)
}
}
\keyword{trends}

