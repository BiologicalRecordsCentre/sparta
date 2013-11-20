\name{sampleRd}
\alias{sampleRd}
\title{Generate a new set of relative distributions based on a given set of relative distributions}
\usage{
  sampleRd(rd, n, rd.names = factor(seq_len(nrow(rd))))
}
\arguments{
  \item{rd}{a matrix with relative distributions. The first
  column contains the relative distribution at the first
  timepoint. The column column those of the second
  timepoint. Other columns are ignored.}

  \item{n}{a vector with the number of sampled gridcells in
  each period. Only the first two are used.}

  \item{rd.names}{an optinal vector with the species names}
}
\value{
  a matrix with four columns and as much rows as in rd. The
  columns contain: the resampled relative distribution at
  timepoint 1, the resampled relative distribution at
  timepoint 2, the index from Maes et al assuming the null
  hypothesis and the observed index from Maes et al based
  on the new sample.
}
\description{
  Generate a new set of relative distributions based on a
  given set of relative distributions
}
\author{
  Thierry Onkelinx
}

