# RKGCluster-Package

Package: RKGCluster
Type: Package
Title: Package for Clustering and a lot more
Version: 0.1.0
Imports: plotly, mclust, purrr, clue, class, factoextra, cluster
Description: This package does Clustering using K-Means and Guassian Mixture Models. All examples in the help pages for each function have used the datasets train and test which have height, weight, age and gender class in both of them. The basic functionality of each function is described below:
    KMeans - Builds a K-Means Clustering Model
    GMMClust - Build a Guassian Mixture Model (GMM)
    plotterK & plotterM - Plots 3D plotly visualizations using K-Means and GMM
    OptimalK - Gives the optimal number of clusters for a dataset for K-Means clustering by the maximum average silhouette coefficeint criteria
    ClassPredictorK & ClassPredictorM - Predicts classes for test data for K-Means and GMM respectively. Also gives confusion matrix and a lot more.
    comparer - compares and tell which works better for the dataset we are having which can be K-Means or Guassian Mixture Model
Author: Rahul Kumar Goyal <rahulkumar.goyal@utdallas.edu>
Maintainer: Rahul Kumar Goyal <rahulkumar.goyal@utdallas.edu>
License: UTD
Encoding: UTF-8
LazyData: true
RoxygenNote: 6.0.1
