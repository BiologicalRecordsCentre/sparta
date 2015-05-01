#' @importFrom reshape2 dcast

RR_model_func <- function(taxa_data, species_name, space_time,
                          overdispersion = FALSE, verbose = FALSE,
                          site_effect = TRUE, list_length = FALSE,
                          model_formula = NULL, family = 'Binomial'){
  
  # Run model and return model object
  MM <- modelBuilder(taxa_data,
                     species_name,
                     space_time,
                     list_length,
                     site_effect,
                     overdispersion,
                     model_formula,
                     family,
                     verbose)  
  
  # Check that it worked
  if(class(MM)[1] == "try-error"){ # If there is an error catch what data we can
    coefs <- list(species_name = as.character(species_name),
                  observations = sum(as.numeric(attr(MM, 'dataset')$taxa)),
                  error_message = MM[1])
  } else { # Create a list of all our parameters
    
    # Turn the coefficients table into a list with nice names
    summary_melt <- melt(summary(MM)$coefficients)    
    summary_melt$name <- reformatCoefNames(paste(summary_melt$Var1, summary_melt$Var2, sep = '.'))
    coefs <- as.list(summary_melt$value)
    names(coefs) <- summary_melt$name
    
    # Add other data to this list
    coefs <- c(species_name = as.character(species_name),
               coefs,
               observations = sum(as.numeric(attr(MM, 'dataset')$taxa)))

  }
  
  return(coefs)

}