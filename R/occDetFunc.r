occDetFunc <- function (taxa_name, occDetdata, spp_vis, n_iterations = 5000, nyr = 3,
                        burnin = 1500, thinning = 3, n_chains = 3, output_dir = getwd(),
                        model.file = occDetBUGScode) {
  
  # Add the focal column (was the species recorded on the visit?). Use the spp_vis dataframe to extract this info
  occDetdata <- merge(occDetdata, spp_vis[,c("visit", taxa_name)])
  names(occDetdata)[names(occDetdata) == taxa_name] <- "focal"
  
  # Record the max and min year
  min_year <- min(occDetdata$year)
  max_year <- max(occDetdata$year)
  
  # year and site need to be numeric starting from 1 to length of them.  This is due to the way the bugs code is written
  occDetdata$year <- occDetdata$year - min(occDetdata$year) + 1
  occDetdata$site <- as.numeric(as.factor(occDetdata$site))
  
  # need to get a measure of whether the species was on that site in that year, unequivocally, in zst
  zst <- acast(occDetdata, site ~ factor(year), value.var = 'focal', max, fill = 0) # initial values for the latent state = observed state
  nyear <- max_year - min_year + 1
  
  # look for missing years
  if(length(unique(occDetdata$year)) != nyear) stop('It looks like you have years with no data. This will crash BUGS')
  
  parameters <- c("fit", "fit.new", "psi.fs", "regres.psi","regres.pdet", "sigma2",
                  "sd.lp", "mu.lp", "tau.lp", "pdet.alpha", "mean_late",
                  "mean_early", "LL.p")
  
  yps <- rowSums(acast(occDetdata, site ~ year, length, value.var = 'L') > 0)
  sites_to_include <- names(yps[yps >= nyr])
  zst <- zst[row.names(zst) %in% sites_to_include,]
  i <- occDetdata$site %in% sites_to_include
  
  # now assemble the bugs_data and related objects
  #need to convert Site identities into row numbers
  site_to_row_lookup <- data.frame(site = as.integer(row.names(zst)),
                                   rownum = 1:nrow(zst)) 
  
  # HERE IS THE BUGS DATA
  bugs_data <- with(merge(occDetdata[i,], site_to_row_lookup), # adds rownum to occDetdata (used below)
                    list(y = as.numeric(focal), Year = year, Site = rownum, 
                         nyear = nyear, nsite = nrow(zst), nvisit = nrow(occDetdata[i,]),
                         sumX = sum(unique(year)), sumX2 = sum(unique(year)^2)))
  
  ### Specifiy additional parameters and parts of the bugs data
  bugs_data$logL = log(occDetdata[i,]$L)
  bugs_data$dtype2p_min = -10
  bugs_data$dtype2p_max = 10 #constraints on the priors
  init = 2 # for defining which initial values are required
  
  initiate <- function(z, i = 1, nyear) {
    init <- list (z = z, alpha.p = rep(runif(1, -2, 2), nyear))
    if(i >= 2) init$LL.p = runif(1, -2, 2)
    init
  }
  
  # set the initial values... 
  init.vals <- replicate(n_chains, initiate(z = zst, i = init, nyear = nyear),
                         simplify = F)

  error_status <- try(
    out <- jags(bugs_data, init.vals, parameters, model.file = model.file,
                n.chains = n_chains, n.iter = n_iterations, n.thin = thinning,
                n.burnin = burnin, DIC = TRUE)
  )
  
  dir.create(path = output_dir, showWarnings = FALSE) # create the top results folder
  
  if (class(error_status) == "try-error" ){
    write.table(taxa_name, file = file.path(output_dir,"failed_spp.txt"),
                append = TRUE, row.names = FALSE, col.names = FALSE)
    return(NULL)
  } else {
    out$SPP_NAME <- taxa_name
    out$min_year <- min_year
    out$max_year <- max_year
    save(out, file = file.path(output_dir, paste(taxa_name, ".rdata", sep = "")))  
    return(file.path(output_dir, paste(taxa_name, ".rdata", sep = "")))
  }  	
}