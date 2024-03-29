% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/amidaKuji.R
\name{amidaKujiCreate}
\alias{amidaKujiCreate}
\title{Create the base of Amida Kuji.}
\usage{
amidaKujiCreate(nVerticalLines = 12, nHorizontalLines = 30,
  nMinBetweenLines = 2, lenVertical = 10, goalIDs = paste0("G",
  1:nVerticalLines), plotKuji = TRUE, noCross = FALSE,
  playerNames = paste0("P", 1:nVerticalLines))
}
\arguments{
\item{nVerticalLines}{The number of vertical lines in Amida Kuji. It should be equal or larger than the number of players.}

\item{nHorizontalLines}{The number of horizontal lines in Amida Kuji.
Of course, more this value increase, more the AMidakuji becomes complex.}

\item{nMinBetweenLines}{The minumum number of lines between each vertical lines.
If this value <= 1, there is a possibility to occur a bias in Amida Kuji.}

\item{lenVertical}{The length of vertical lines. This parameter should not influence the results.}

\item{goalIDs}{The IDs for goals. The length of this argument should be equal to `nVerticalLines`.}

\item{plotKuji}{If TRUE, plot Amida Kuji in this function.}

\item{noCross}{If TRUE, there will be no cross in AMida Kuji. The default is FALSE}

\item{playerNames}{The names of players. The length of this argument should be equal to `nVerticalLines`.}
}
\value{
Nothing, but create information about Amida Kuji as global objects.
}
\description{
This function creates the base of Amida Kuji by determining where the horizontal lines are put.
}
