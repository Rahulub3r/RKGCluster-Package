% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GMMClust.R
\name{GMMClust}
\alias{GMMClust}
\title{Model-Based Clustering}
\usage{
GMMClust(data, G = NULL)
}
\arguments{
\item{data:}{A data matrix}

\item{G:}{An interger specifying the number of mixture components for which BIC is to be calculated. The default is G=1:9}
}
\value{
GMMClust returns an object of class \code{'Mclust'}. To see what components it has use the command \code{names(gmmclust)} where \code{gmmclust} is an object of the class \code{Mclust}
}
\description{
Perform clustering by Gaussian Mixture Model on a data matrix
}
\examples{
#The example below takes age, height and gender and, produces a Guassian Mixture Model
train = read.csv('train.csv')
GMMClust(train[,1:3])
}
