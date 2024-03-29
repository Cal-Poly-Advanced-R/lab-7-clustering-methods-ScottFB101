% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/k_means.R
\name{k_means}
\alias{k_means}
\title{Implements K means by hands}
\usage{
k_means(dat, k, pca = FALSE, k_means_plus = TRUE)
}
\arguments{
\item{dat}{A data set}

\item{k}{The number of desired clusters}

\item{pca}{Boolean value indicating whether or not to run Principal Component Analysis prior to K means}

\item{k_means_plus}{Boolean value indicating whether or not to choose the initial clusters in a "smart" way}
}
\value{
Returns cluster assignments, the mean centroid of each cluster, the number of iterations to
achieve the clustering, and the total sum of squares of the data to the mean centroid of the data set.
}
\description{
Implements K means by hands
}
