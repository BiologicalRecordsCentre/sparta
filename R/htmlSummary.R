#' Create HTML Report
#' 
#' Create HTML Report for an \code{occDet} object.
#' 
#' @param occDet An object of class occDet
#' @param open Logical, if \code{TRUE} the html report is opened once complete
#' @param output_dir Character, the directory where the html file will be saved,
#' defaults to the working directory.
#' @param output_file (Optional) The file name given to the html file
#' @param ... Additional arguments passed to rmarkdown::render
#' 
#' @return Path to html report
#' @import rmarkdown
#' 
#' @examples
#' 
#' \dontrun{
#' # Create data
#' set.seed(125)
#' n <- 15000 #size of dataset
#' nyr <- 20 # number of years in data
#' nSamples <- 100 # set number of dates
#' nSites <- 50 # set number of sites
#' 
#' # Create somes dates
#' first <- as.Date(strptime("1980/01/01", "%Y/%m/%d")) 
#' last <- as.Date(strptime(paste(1980+(nyr-1),"/12/31", sep=''), "%Y/%m/%d")) 
#' dt <- last-first 
#' rDates <- first + (runif(nSamples)*dt)
#' 
#' # taxa are set as random letters
#' taxa <- sample(letters, size = n, TRUE)
#' 
#' # three sites are visited randomly
#' site <- sample(paste('A', 1:nSites, sep=''), size = n, TRUE)
#' 
#' # the date of visit is selected at random from those created earlier
#' survey <- sample(rDates, size = n, TRUE)
#' 
#' # run the model with these data for one species
#' # using defaults
#' results <- occDetModel(taxa = taxa,
#'                        site = site,
#'                        survey = survey,
#'                        species_list = 'a',
#'                        write_results = TRUE,
#'                        n_iterations = 1000,
#'                        burnin = 10,
#'                        thinning = 2)
#' 
#' generate summary
#' htmlSummary(results$a)
#' 
#' }
#' 
#' @export

htmlSummary <- function(occDet,
                        open = TRUE,
                        output_dir = getwd(),
                        output_file = NULL,
                        ...){
  
  html_template <- system.file("templates/Occ_viz_html.Rmd", package = "sparta")
  stopifnot(inherits(occDet, 'occDet'))
  temp_f <- tempfile()
  saveRDS(occDet, file = temp_f)
  
  output_path <- render(html_template,
                        params = list(dataFile = temp_f),
                        output_dir = output_dir,
                        output_file = output_file,
                        intermediates_dir = tempdir(),
                        quiet = TRUE,
                        ...)
  
  if(open) browseURL(output_path)
  return(output_path)
  
}