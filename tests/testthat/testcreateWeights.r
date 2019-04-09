context("Test createWeights")

#small dataset
library(sp)
library(reshape2)
set.seed(seed = 128)
n <- 10 #number of sites
sites <- paste('A',1:n, sep='')
locations <- data.frame(sites, x = runif(n, min=0, max=1000), y = runif(n, min=0, max=1000))
x <- as.matrix(locations[c('x','y')])  
dist <- spDists(x, y=x,longlat=FALSE)
row.names(dist) <- locations$site
colnames(dist) <- locations$site
dist <- melt(dist)
sim <- data.frame(SQ_SQUARE = sites)
sim[,2:11] <- runif(n*10, min=0, max=10)

temp <- capture.output(weights <- createWeights(dist, sim, dist_sub = 5, sim_sub = 3, verbose = FALSE))

test_that("Test errors and warnings", {
  
  expect_warning(createWeights(dist, sim[1:5,], dist_sub = 5, sim_sub = 3,
                               verbose = FALSE),
                 "The following sites were in only one of 'attributes' and 'distances' and so have been excluded from the weights file")
  expect_error(createWeights(head(dist, -1), sim, dist_sub = 5, sim_sub = 3,
                             verbose = FALSE),
               'dist table does not include all possible combinations of sites')  
  expect_error(createWeights(dist, sim, dist_sub = 5, sim_sub = 5, verbose = FALSE),
               "dist_sub' cannot be smaller than or equal to 'sim_sub'")  
  expect_error(createWeights(dist, sim, dist_sub = 5, sim_sub = 10, verbose = FALSE),
               "dist_sub' cannot be smaller than or equal to 'sim_sub'") 
  expect_error(createWeights(TRUE, sim, dist_sub = 5, sim_sub = 3, verbose = FALSE),
               "dist must be a data.frame") 
  expect_error(createWeights(dist, 'Tom', dist_sub = 5, sim_sub = 3, verbose = FALSE),
               "sim must be a data.frame") 
  expect_error(createWeights(cbind(dist, 1:nrow(dist)), sim, dist_sub = 5,
                             sim_sub = 3, verbose = FALSE),
               "dist must have three columns")
  expect_error(createWeights(cbind(dist[,1:2], as.character(dist[,3])), sim,
                             dist_sub = 5, sim_sub = 3, verbose = FALSE),
               "the value column in dist must be an integer or numeric") 
  expect_error(createWeights(dist, cbind(sim[1:10], as.character(sim[,11])),
                             dist_sub = 5, sim_sub = 3, verbose = FALSE),
               'the values in sim must be integers or numeric') 
  
})

test_that("Test createWeights functionality", {
  
  expect_equal(nrow(weights), 3 * length(sites))
  expect_true(all(weights$weight[weights$target == weights$neighbour] == 1))
  
})