#' 3D plotter
#'
#' Plots a 3D plot on a data matrix as a plotly visualization
#'
#' @param data A dataset with 3 columns to plot
#' @param fill A vector by which data is to be color coded
#' @return plotter returns a plot of class \code{plotly}
#' @examples
#' #The below example is plotting the age, height and weight color coded by gender
#' train = read.csv('train.csv')
#' plotter(train[,1:3], fill = train[,5])
#' @export
plotter <- function(data = data, fill){
  require(plotly)
  tempdata = data
  tempdata$cluster = as.factor(fill)

  p <- plot_ly(tempdata, x = ~tempdata[,1], y = ~tempdata[,2], z = ~tempdata[,3], color = ~cluster, colors = c('#BF382A', '#0C4B8E')) %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = colnames(tempdata)[1]),
                        yaxis = list(title = colnames(tempdata)[2]),
                        zaxis = list(title = colnames(tempdata)[3])))
  return(p)
}
#' K-Means 3D plotter
#'
#' Plots a 3D plot on a data matrix using K-Means Clustering as a plotly visualization
#'
#' @param data A dataset with 3 columns to plot
#' @param k The number of clusters
#' @return plotterK returns a plot of class \code{plotly}
#' @examples
#' #The below example is plotting the age, height and weight color coded by cluster
#' train = read.csv('train.csv')
#' plotterK(train[,1:3],2)
#' @export
plotterK <- function(data, k){
  require(plotly)

  KMeans = kmeans(data, k)
  tempdata = data

  x = data.frame(KMeans$centers)

  x$cluster = paste0('Cluster number ', seq(1:k))
  tempdata$cluster = as.factor(KMeans$cluster)

  tempdata = rbind(tempdata, x)

  p <- plot_ly(tempdata, x = ~tempdata[,1], y = ~tempdata[,2], z = ~tempdata[,3], color = ~cluster, colors = c('#BF382A', '#0C4B8E')) %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = colnames(tempdata)[1]),
                      yaxis = list(title = colnames(tempdata)[2]),
                      zaxis = list(title = colnames(tempdata)[3])))
  return(p)
}

#' Guassian Mixture Model 3D plotter
#'
#' Plots a 3D plot on a data matrix using Guassian Mixture Model Clustering as a plotly visualization
#'
#' @param data A dataset with 3 columns to plot
#' @param k The number of clusters
#' @return plotterM returns a plot of class \code{plotly}
#' @examples
#' #The below example is plotting the age, height and weight color coded by cluster
#' train = read.csv('train.csv')
#' plotterM(train[,1:3],7)
#' @export
plotterM <- function(data, k){
  require(plotly)
  require(mclust)

  GMM = Mclust(data, k)

  tempdata = data
  tempdata$cluster = as.factor(GMM$classification)

  p <- plot_ly(tempdata, x = ~tempdata[,1], y = ~tempdata[,2], z = ~tempdata[,3], color = ~cluster, colors = c('#BF382A', '#0C4B8E')) %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = colnames(tempdata)[1]),
                        yaxis = list(title = colnames(tempdata)[2]),
                        zaxis = list(title = colnames(tempdata)[3])))
  return(p)
}
