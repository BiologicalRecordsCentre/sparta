Convert_records_to_2tp <-
function(records, splityr) {
  # takes raw biological records and generates a dataframe suitable for Telfer etc
  n1 <- with(subset(records, year <= splityr), tapply(hectad, CONCEPT, LenUniq)) # number of sites for each species in t1
  n2 <- with(subset(records, year > splityr), tapply(hectad, CONCEPT, LenUniq))
  d1 <- LenUniq(subset(records, year <= splityr)$hectad) # total number of sites in t1
  d2 <- LenUniq(subset(records, year > splityr)$hectad) # total number of sites in t2
  gridcell_counts <- data.frame(n1,n2)
  attr(gridcell_counts, 'denom') <- c(d1,d2)
  return(gridcell_counts)
}
