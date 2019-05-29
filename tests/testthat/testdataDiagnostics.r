context("Test dataDiagnostics")

# Create data
n <- 15000 #size of dataset
nyr <- 20 # number of years in data
nSamples <- 100 # set number of dates
nSites <- 50 # set number of sites
set.seed(125)

# Create somes dates
first <- as.Date(strptime("2010/01/01", "%Y/%m/%d")) 
last <- as.Date(strptime(paste(2010+(nyr-1),"/12/31", sep=''), "%Y/%m/%d")) 
dt <- last-first 
rDates <- first + (runif(nSamples)*dt)

# taxa are set as random letters
taxa <- sample(letters, size = n, TRUE)

# three sites are visited randomly
site <- sample(paste('A', 1:nSites, sep=''), size = n, TRUE)

# the date of visit is selected at random from those created earlier
time_period <- sample(rDates, size = n, TRUE)

df <- data.frame(taxa, site, time_period)

test_that("Test dataDiagnostics", {
  
 suppressWarnings(results <- dataDiagnostics(taxa = df$taxa,
                            progress_bar = FALSE,
                            site = df$site,
                            time_period = df$time_period,
                            plot = FALSE))
 
 recperyear <- structure(c(771L, 775L, 882L, 303L, 443L, 1051L, 582L, 862L, 
             591L, 324L, 768L, 751L, 728L, 924L, 739L, 900L, 738L, 939L, 1185L, 
             744L), .Dim = 20L, .Dimnames = structure(list(RecordsPerYear = c("2010", 
                                                                              "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", 
                                                                              "2019", "2020", "2021", "2022", "2023", "2024", "2025", "2026", 
                                                                              "2027", "2028", "2029")), .Names = "RecordsPerYear"), class = "table")
 visittop50 <- structure(list(time_period = structure(c(14657, 14657, 14657, 
                                                         14657, 14657, 14657, 14657, 14657, 14657, 14657, 14657, 14657, 
                                                         14657, 14657, 14657, 14657, 14657, 14657, 14657, 14657, 14657, 
                                                         14657, 14657, 14657, 14657, 14657, 14657, 14657, 14657, 14657, 
                                                         14657, 14657, 14657, 14657, 14657, 14657, 14657, 14657, 14657, 
                                                         14657, 14657, 14657, 14657, 14657, 14657, 14657, 14657, 14713, 
                                                         14713, 14713), class = "Date"),
                site = structure(c(1L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 
                                   11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 
                                   24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 34L, 35L, 36L, 37L, 
                                   39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 1L, 
                                   2L, 3L), .Label = c("A1", "A10", "A11", "A12", "A13", "A14", 
                                                                     "A15", "A16", "A17", "A18", "A19", "A2", "A20", "A21", "A22", 
                                                                     "A23", "A24", "A25", "A26", "A27", "A28", "A29", "A3", "A30", 
                                                                     "A31", "A32", "A33", "A34", "A35", "A36", "A37", "A38", "A39", 
                                                                     "A4", "A40", "A41", "A42", "A43", "A44", "A45", "A46", "A47", 
                                                                     "A48", "A49", "A5", "A50", "A6", "A7", "A8", "A9"), class = "factor"),  
                listLength = c(1L, 5L, 5L, 1L, 3L, 3L, 1L, 1L, 1L, 2L, 2L, 
                               5L, 6L, 4L, 1L, 5L, 1L, 5L, 4L, 5L, 3L, 7L, 2L, 4L, 5L, 2L, 
                               2L, 5L, 1L, 6L, 5L, 1L, 2L, 2L, 4L, 3L, 3L, 3L, 2L, 1L, 5L, 
                               2L, 1L, 5L, 2L, 2L, 3L, 5L, 5L, 1L)), .Names = c("time_period", 
                                                                                "site", "listLength"), row.names = c(NA, 50L), class = "data.frame")

 coefModRecs <- structure(c(-26329.52105, 13.40902), .Names = c("(Intercept)", 
                                                              "time_period"))
 coefModVis <- structure(c(1.088671e+00, 6.040089e-07), .Names = c("(Intercept)", 
                                                                               "time_period"))
 
     
 expect_equal(recperyear, results$RecordsPerYear)
 expect_equal(visittop50, results$VisitListLength[1:50,])
 expect_equal(coefModRecs, coefficients(results$modelRecs))
 expect_equal(coefModVis, coefficients(results$modelList), tolerance = 1e-6)
  
})