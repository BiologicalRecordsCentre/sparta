#' Calculate dataset metrics
#'
#' Takes a formattedOccData object and the name of a species. Runs a set of calculations on the dataset for that species
#' Based on rules of thumb. Original version by Mark Logie; integrated into sparta by NJBI
#'  

#' @param sp character string identifying the species name
#' @param formattedData output from formattedOccData
#' @param suffix Not used
#' 
#' @references Pocock, Logie, Isaac, Outhwaite & August. Rapid assessment of the suitability of multi-species citizen science datasets for occupancy trend analysis. bioRxiv 813626 (2019) doi:10.1101/813626.
#' @export


dataMetrics <- function(sp, formattedData, suffix=NULL){
  
    tFDall <- merge(formattedData$occDetdata,
                              formattedData$spp_vis)
    tFD <- tFDall[tFDall[,names(tFDall) == sp],]
    
    if(nrow(tFD)==0){
      # No data, so set a few variables to 0.  The reason for using 0's as defaults is if
      # there is no data, these parameters should be 0 e.g. median number of visits = 0.
      # Without this default, many parameters return NA, which is less accurate.
      nyears <- Spnsite <- Spnvisits <- SpvisitPerSite <- SpvisitPerYear <-
        avVisitPerYear <- median <- P70 <- P80 <- P90 <- sdVisitsPerYear <-
        coeffVar <- zmedian <- zP70 <- zP80 <- zP90 <- zsdVisitsPerYear <-
        zcoeffVar <- visits_median <- visits_P70 <- visits_P80 <- visits_P90 <-
        prop_repeats_spc <- prop_repeats_grp <- prop_of_years <- prop_list_one <-
        prop_abs_list <- Totalnvisits <- Totalnsites <- TotalvisitPerYear <- 0
      prop_abs <- 1
    } else {
      # range of years with data
      nyears <- (1+max(tFD$TP)-min(tFD$TP))
      # Number of sites species is observed at
      Spnsite <- length(unique(tFD$site))
      # Number of visits on which the species is observed
      Spnvisits <- length(unique(tFD$visit))
      # Number of visits per site
      SpvisitPerSite <- Spnvisits/Spnsite
      # Number of visits per year
      SpvisitPerYear <- Spnvisits/nyears
      
      # average visits per year
      visits_year <- tapply(tFD$visit, tFD$TP,
                            FUN = function(x) length(unique(x)))
      avVisitPerYear = mean(visits_year)
      
      # find 70, 80 and 90th percentiles
      percentiles <- quantile(visits_year,c(.5,.7,.8,.9))
      median <- as.numeric(percentiles[1])
      P70 <- as.numeric(percentiles[2])
      P80 <- as.numeric(percentiles[3])
      P90 <- as.numeric(percentiles[4])
      
      if(nrow(tFD)==1){
        sdVisitsPerYear <- coeffVar <- 1
      } else {
        # sd visits per year
        sdVisitsPerYear = sd(visits_year)
        # coefficient of variation
        coeffVar <- sd(visits_year)/mean(visits_year)
      }
      
      unique_years <- unique(tFD$TP)
      all_years    <- min(tFDall$TP):max(tFDall$TP)
      missing_years <-  all_years[!(all_years %in% unique_years)]
      visits_year <- c(visits_year,rep(0,length(missing_years)))
      
      # find 70, 80 and 90th percentiles
      percentiles <- quantile(visits_year,c(.5,.7,.8,.9))
      zmedian <- as.numeric(percentiles[1])
      zP70 <- as.numeric(percentiles[2])
      zP80 <- as.numeric(percentiles[3])
      zP90 <- as.numeric(percentiles[4])
      
      # sd visits per year
      zsdVisitsPerYear = sd(visits_year)
      # coefficient of variation
      zcoeffVar <- sd(visits_year)/mean(visits_year)
      
      # repeat visits
      # Within each year get the counts of visits to each location
      #repeats <- count(tFD, site, TP) # throws an error
      repeats <- subset(as.data.frame(with(tFD, table(site, TP))), Freq > 0)
      repeats$concat <- paste0(repeats$site,'-',repeats$TP)
      
      # What proportion of these are > 1
      prop_repeats_spc <- sum(repeats$Freq > 1) / nrow(repeats)
      
      # visits within the group for visits to each location within a year
      #group_repeats <- count(tFDall, site, TP)
      group_repeats <- subset(as.data.frame(with(tFDall, table(site, TP))), Freq > 0)
      group_repeats$concat <-
        paste0(group_repeats$site,'-',group_repeats$TP)
      
      # Find which of these group visits are to sites in years where the
      # species of interest was observed
      site_year <- group_repeats$concat %in% repeats$concat
      group_repeats <- group_repeats$Freq[site_year]
      prop_repeats_grp <- sum(group_repeats > 1) / length(group_repeats)
      
      # all visits to a site in a year where there was at least one observance
      # of species of interest
      percentiles <- quantile(group_repeats,c(.5,.7,.8,.9))
      visits_median <- as.numeric(percentiles[1])
      visits_P70 <- as.numeric(percentiles[2])
      visits_P80 <- as.numeric(percentiles[3])
      visits_P90 <- as.numeric(percentiles[4])
      
      # proportion of years with data
      prop_of_years <-
        length(unique(tFD$TP))/(1+max(tFDall$TP)-min(tFDall$TP))
      
      # lists of length 1
      prop_list_one <- sum(tFD$L == 1) / nrow(tFD)
    }
    
    # proportion of records for the group which did not observe this species
    prop_abs <- (nrow(tFDall)-nrow(tFD))/(nrow(tFDall))
    
    # proportion of non-observances with list length > 1
    tFDabs <- tFDall[!tFDall[,colnames(tFDall) %in% sp],]
    prop_abs_list <- sum(tFDabs$L > 1) / nrow(tFDabs)
    
    # Number of sites for taxonomic group
    Totalnsites <- length(unique(tFDall$site))
    # Number of visits for taxonomic group
    Totalnvisits <- nrow(tFDall)
    if(nrow(tFDall)==0){
      total_years <- 0
      TotalvisitPerYear <- 0
    } else {
      total_years <- length(min(tFDall$TP):max(tFDall$TP))
      TotalvisitPerYear <- Totalnvisits/total_years
    }
    
    df <- data.frame(species = sp,
                     avVisitPerYear = avVisitPerYear,
                     median = median,
                     P70 = P70,
                     P80 = P80,
                     P90 = P90,
                     sdVisitsPerYear = sdVisitsPerYear,
                     coeffVar = coeffVar,
                     zmedian = zmedian,
                     zP70 = zP70,
                     zP80 = zP80,
                     zP90 = zP90,
                     zsdVisitsPerYear = zsdVisitsPerYear,
                     zcoeffVar = zcoeffVar,
                     prop_repeats_spc = prop_repeats_spc,
                     prop_repeats_grp = prop_repeats_grp,
                     visits_median = visits_median,
                     visits_P70 = visits_P70,
                     visits_P80 = visits_P80,
                     visits_P90 = visits_P90,
                     prop_of_years = prop_of_years,
                     prop_list_one = prop_list_one,
                     prop_abs = prop_abs,
                     prop_abs_list = prop_abs_list,
                     nyears = nyears,
                     Spnsite = Spnsite,
                     Spnvisits = Spnvisits,
                     SpvisitPerSite = SpvisitPerSite,
                     SpvisitPerYear = SpvisitPerYear,
                     Totalnsites = Totalnsites,
                     Totalnvisits = Totalnvisits,
                     TotalvisitPerYear = TotalvisitPerYear,
                     stringsAsFactors = FALSE)
  
  return(df)
}
