#set seed for reproducibility
set.seed(12345)
# sample size for both timepoints
n <- c(2000, 10000)
# generate some true rd values
rd.truth <- matrix(runif(50), ncol = 2)
rd.truth <- t(apply(rd.truth, 1, function(x){
  x / colSums(rd.truth)
}))
# calculate the true trend
rd.truth <- cbind(rd.truth, (rd.truth[, 2] - rd.truth[, 1]) / rd.truth[, 1])


# test the Type I error under the null hypothesis
test.null <- replicate(1e3, {
  # generate a realisation of rd based on the true rd assuming no trend (rd[, 1] == rd[, 2])
  rd <- sampleRd(rd = rd.truth[, c(1, 1)], n = n)[, c(1, 3, 5)]
  # calculate the two sided p values for the trend
  twoSidedPValue(rd = rd, n = n)
})
#under the null hypothesis 5% of the trends should have a p value lower than 0.05
mean(test.null <.05, na.rm = TRUE)


# test the coverage of the confidence intervals
test.ci <- replicate(1e3, {
  # generate a realisation of rd based on the true rd
  rd <- sampleRd(rd = rd.truth, n = n)[, c(1, 3, 5)]
  # calculate the confidence interval
  x <- confintRd(rd = rd, n = n)
  # test if the true rd value is within the confidence interval
  rd.truth[, 3] >= x[, "lcl"] & rd.truth[, 3] <= x[, "ucl"]
})
# 95% of the confidence intervals should contains the true rd value
rowMeans(test.ci, alpha = 0.05)


# estimate the power for detecting trends
test.power <- replicate(1e3, {
  # generate a realisation of rd based on the true rd
  rd <- sampleRd(rd.truth, n = n)[, c(1, 3, 5)]
  # calculate the two sided p values for the trend
  twoSidedPValue(rd)
})
# the power should be 5% under the null hypothesis (no trend). In that case log(1 + rd.truth[, 3]) is 0
# the trend should increase monotonic when the absolute value of the trend increases. It must reach 100%
power <- rowMeans(test.power < 0.05, na.rm = TRUE)
plot(log(1 + rd.truth[, 3]), power)


# test if the sample size affects the p values
test.n <- expand.grid(
  n1 = c(100, 500, 1000, 5000),
  n2 = c(100, 500, 1000, 5000)
)
apply(test.n, 1, function(n){
  # get p value 
  twoSidedPValue(rd.truth)
})
