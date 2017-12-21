#' Model-Based Clustering
#'
#' Perform clustering by Gaussian Mixture Model on a data matrix
#'
#' @param data: A data matrix
#' @param G: An interger specifying the number of mixture components for which BIC is to be calculated. The default is G=1:9
#' @return GMMClust returns an object of class \code{'Mclust'}. To see what components it has use the command \code{names(gmmclust)} where \code{gmmclust} is an object of the class \code{Mclust}
#' @examples
#' #The example below takes age, height and gender and, produces a Guassian Mixture Model
#' train = read.csv('train.csv')
#' GMMClust(train[,1:3])
#' @export
GMMClust <- function(data, G = NULL){
  return(Mclust(data, G))
}
