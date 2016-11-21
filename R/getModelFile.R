getModelFile <- function(modeltype, verbose = TRUE){
  
  # first get all the available model files
  modelFiles <- list.files(system.file("models", package = "sparta"),
                           full.names = TRUE)
  # fileLocation <- '//nercwlctdb.ad.nerc.ac.uk/shared/SHARED1/PYWELL_SHARED/Pywell Projects/BRC/Charlie/0. Model Testing/2. Final models and function scripts'
  # modelFiles  <- list.files(path = fileLocation, full.names = TRUE, pattern = '^SPARTA_')
  
  # Now break them down
  mods <- gsub('^SPARTA_', '', basename(modelFiles))
  mods <- gsub('.txt', '', tolower(mods))
  mods <- strsplit(mods, split = '_')
  
  # match only attibutes that effect model file selection
  modeltype_file <- modeltype[!tolower(modeltype) %in% c('catlistlength', 'jul_date',
                                                         'contlistlength')]
  
  matches <- lapply(mods, FUN = function(x) all(modeltype_file %in% x) & all(x %in% modeltype_file))
  
  # No match
  if(all(!unlist(matches))){
    stop('There are no model files that match your combination: ',
         paste(modeltype_file, collapse = ', '))
  } else if(sum(unlist(matches) > 1)){
    stop('There is more than one model file that matches:',
         paste(mods[unlist(matches)]))
  } else if(sum(unlist(matches) == 1)){
    modelfile <- modelFiles[unlist(matches)]
    if(verbose) cat('Selected model file:', basename(modelfile), '\n')
  }
  
  ## read in and adapt model before parsing and returning as an expression ##
  modelScript <- readLines(modelfile)
  
  obsModel <- getObsModel(modeltype, verbose)
  
  tFile <- tempfile(fileext = '.txt')
  
  fullModel <- paste('model{\n',
                    paste(modelScript, collapse = '\n'),
                    obsModel,
                   '}')
  
  write.table(x = fullModel, file = tFile,
              row.names = FALSE, col.names = FALSE,
              quote = FALSE)
  
  return(tFile)
  
}