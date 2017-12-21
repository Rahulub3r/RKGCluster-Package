#' Comparer
#'
#' Compares two objects of classes \code{ClassPredictorK} and \code{ClassPredictorM}
#' @param lk An object of class \code{ClassPredictorK} which can be built using a function in this package
#' @param lm An object of class \code{ClassPredictorM} which can be built using a function in this package
#' @return Prints which model is better for the specific dataset working
#' @examples
#' train = read.csv('train.csv')
#' test = read.csv('test.csv')
#' lk = ClassPredictorK(train[,1:3], test[1:3], 2, train[,5],test[,5])
#' lm = ClassPredictorM(train[,1:3], test[,1:3],7, train[,5], test[,5])
#' comparer(lk, lm)
#' @export

comparer <- function(lk, lm){
  if (lk$accuracy > lm$accuracy){
    print('K-Means works better for this dataset')
  }
  if (lk$accuracy == lm$accuracy){
    print('Both work equally for this dataset')
  }
  if (lk$accuracy < lm$accuracy){
    print('Guassian Mixture Model works better for this dataset')
  }
}
