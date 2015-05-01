#' @import lme4
#' @importFrom reshape2 dcast
#' @importFrom reshape2 acast

modelBuilder <- function(taxa_data, species_name, space_time,
                         list_length = FALSE, site_effect = FALSE,
                         overdispersion = FALSE, model_formula = NULL,
                         family = 'Binomial', verbose = FALSE){
  
    
  # Get data for just this species as 0 and 1 against visits
  y <- subset(taxa_data, taxa == species_name)
  spDat <- merge(x = space_time, y = y, all.x = T, all.y = F)
  spDat$taxa <- as.character(spDat$taxa)
  spDat$taxa[is.na(spDat$taxa)] <- 0
  spDat$taxa[spDat$taxa == species_name] <- 1
  spDat$taxa <- as.numeric(spDat$taxa)
  # Add data for overdispersion
  spDat$obs <- 1:nrow(spDat)
  

  if(!list_length){
    # Summarise each site~year as a row
    MMdata <- dcast(spDat, year + site ~ ., fun = length, value.var = 'listLength') #how many lists for each year?
    # This has created a number of visits column
    names(MMdata)[ncol(MMdata)] <- 'nVis'  
    # Number of successful visits for our species
    MMdata$successes <- as.numeric(acast(spDat, year + site ~ ., fun = sum, value.var='taxa'))
    # Number of unsuccessful visits for our species
    MMdata$failures <- with(MMdata, nVis - successes)
    # Add data for overdispersion
    MMdata$obs <- 1:nrow(MMdata)
  }
  
  if(is.null(model_formula)){ 
    # Build the formula
    model_formula <- formulaBuilder(family,
                                    list_length,
                                    site_effect,
                                    overdispersion)
  }
  
  # We only use the mixed model if we have random effects
  # The formula + data used specifies if it is binomial or bernoulli
  if(site_effect | overdispersion){
    MM <- try(glmer(as.formula(model_formula),  
                    data = if(list_length) spDat else MMdata,
                    family = binomial,
                    verbose = verbose),
              silent = TRUE)
  } else { #else we use glm
    MM <- try(glm(as.formula(model_formula),
                  data = if(list_length) spDat else MMdata,
                  family = binomial),
              silent = TRUE)      
  }
  
  # Return the data too
  attr(MM, 'dataset') <- spDat
  return(MM)
  
}