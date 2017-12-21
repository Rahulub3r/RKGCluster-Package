#' Optimal Number of Clusters
#'
#' Find optimal number of clusters for a data matrix using \code{kmeans} clustering and average silhouette coefficient
#'
#' @param data A dataset for clustering
#' @param till A numeric value which tests from \code{2} to \code{till}
#' @return optimalK returns a numeric value
#' @examples
#' #The example below takes in age, height and weight columns from the train data
#' #then tries values till 15 and gives an output of 2
#' train = read.csv('train.csv')
#' optimalK(data = train[,1:3], till = 15) #returns 2
#' @export
optimalK <- function(data = data, till = 15){

  avg_sil_values = c()

  require(cluster)

  for(i in 2:till){
    km.res = kmeans(data, centers = i)
    ss = silhouette(km.res$cluster, dist(data))
    avg_sil_values = c(avg_sil_values, mean(ss[,3]))
  }

  return(which.max(avg_sil_values)+1)
}

#' Plot Optimal Number of Clusters
#'
#' Find optimal number of clusters for a data matrix using \code{kmeans} clustering and average silhouette coefficient
#'
#' @param data A dataset for clustering
#' @param FUNcluster The clustering algorithm which you want to use. Thee default is \code{kmeans}
#' @param method The method to find optimal clusters which may be silhouette, elbow or gap-statistic
#' @return optimalKplot returns a plot visualization
#' @examples
#' #The example below takes in the age, height and weight and gives a plot based on average
#' #silhouette, suggesting 2 clusters
#' train = read.csv('train.csv')
#' optimalKplot(data = train[,1:3], kmeans, 'silhouette')
#' @export
optimalKplot <- function(data = data, FUNcluster = kmeans, method = c('silhouette')){
  require(factoextra)
  return(fviz_nbclust(data, kmeans, method = method))
}
