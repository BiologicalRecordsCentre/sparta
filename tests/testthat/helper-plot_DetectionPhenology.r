# Create data
n <- 15000 # size of dataset
nyr <- 20 # number of years in data
nSamples <- 100 # set number of dates
nSites <- 50 # set number of sites
set.seed(125)

# Create somes dates
first <- as.Date(strptime("2010/01/01", format = "%Y/%m/%d"))
last <- as.Date(strptime(paste(2010 + (nyr - 1), "/12/31", sep = ""),
    format = "%Y/%m/%d"
))
dt <- last - first
rDates <- first + (runif(nSamples) * dt)

# taxa are set as random letters
taxa <- sample(letters, size = n, TRUE)

# three sites are visited randomly
site <- sample(paste("A", 1:nSites, sep = ""), size = n, TRUE)

# the date of visit is selected at random from those created earlier
survey <- sample(rDates, size = n, TRUE)