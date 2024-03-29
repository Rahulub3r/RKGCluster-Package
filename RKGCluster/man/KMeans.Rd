% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KMeans.R
\name{KMeans}
\alias{KMeans}
\title{K_Means Clustering}
\usage{
KMeans(data, k, nstart = 1, iter.max = 10)
}
\arguments{
\item{data:}{A data matrix}

\item{k:}{Number of Clusters}

\item{nstart:}{Number of random starts}

\item{iter.max:}{number of maximum iterations}
}
\value{
KMeans returns an object of class \code{'kmeans'} which has a \code{print} and \code{fitted} method. To see what components it has use the command \code{names(kmdata)} where \code{kmdata} is an object of the class \code{kmeans}
}
\description{
Perform K-Means clustering on a data matrix
}
\examples{
#The example below takes in age, height and weight columns and segments them into two clusters
train = read.csv('train.csv')
KMeans(train[,1:3], 2)
}
