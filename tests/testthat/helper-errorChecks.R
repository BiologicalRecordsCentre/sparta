# Create data
n <- 3000 # size of dataset
nyr <- 10 # number of years in data
nSamples <- 30 # set number of dates
nSites <- 15 # set number of sites

# Create somes dates
first <- as.POSIXct(strptime("2010/01/01", "%Y/%m/%d"))
last <- as.POSIXct(strptime(paste(2010 + (nyr - 1), "/12/31", sep = ""), "%Y/%m/%d"))
dt <- last - first
rDates <- first + (runif(nSamples) * dt)

# taxa are set as random letters
taxa <- sample(letters, size = n, TRUE)

# three sites are visited randomly
site <- sample(paste("A", 1:nSites, sep = ""), size = n, TRUE)

# the date of visit is selected at random from those created earlier
time_period <- sample(rDates, size = n, TRUE)
time_period_missing <- sample(c(rDates, NA), size = n, TRUE)

dist_sub <- rnorm(n, 10, 1)
sim_sub <- rnorm(n, 10, 1)

dist_sub_fac <- as.factor(rnorm(n, 10, 1))
sim_sub_fac <- as.factor(rnorm(n, 10, 1))


dist_sub_chr <- as.character(rnorm(n, 10, 1))
sim_sub_chr <- as.character(rnorm(n, 10, 1))

dist_sub_fac <- as.factor(rnorm(n, 10, 1))
sim_sub_fac <- as.factor(rnorm(n, 10, 1))


# combine this to a dataframe
df <- data.frame(
  taxa = taxa,
  site = site,
  time_period = as.character(time_period),
  time_period_missing = time_period_missing,
  dist_sub = dist_sub,
  sim_sub = sim_sub,
  dist_sub_chr = dist_sub_chr,
  sim_sub_chr = sim_sub_chr,
  dist_sub_fac = dist_sub_fac,
  sim_sub_fac = sim_sub_fac
)
