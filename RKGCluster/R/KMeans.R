#' K_Means Clustering
#'
#' Perform K-Means clustering on a data matrix
#'
#' @param data: A data matrix
#' @param k: Number of Clusters
#' @param nstart: Number of random starts
#' @param iter.max: number of maximum iterations
#' @return KMeans returns an object of class \code{'kmeans'} which has a \code{print} and \code{fitted} method. To see what components it has use the command \code{names(kmdata)} where \code{kmdata} is an object of the class \code{kmeans}
#' @examples
#' #The example below takes in age, height and weight columns and segments them into two clusters
#' train = read.csv('train.csv')
#' KMeans(train[,1:3], 2)
#' @export
KMeans <- function(data, k, nstart = 1, iter.max = 10){
  return(kmeans(data, k, nstart = nstart, iter.max = iter.max))
}
