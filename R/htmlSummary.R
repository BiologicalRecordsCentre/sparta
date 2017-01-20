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
#' @export
#' @import rmarkdown

htmlSummary <- function(occDet,
                        open = TRUE,
                        output_dir = getwd(),
                        output_file = NULL,
                        ...){
  
  html_template <- system.file("templates/Occ_viz_html.Rmd", package = "sparta")
  stopifnot(inherits(occDet, 'occDet'))
  temp_f <- tempfile()
  saveRDS(occDet, file = temp_f)
  
  output_path <- rmarkdown::render(html_template,
                                   params = list(dataFile = temp_f),
                                   output_dir = output_dir,
                                   output_file = output_file,
                                   intermediates_dir = tempdir(),
                                   quiet = TRUE,
                                   ...)
  
  if(open) browseURL(output_path)
  return(output_path)
  
}