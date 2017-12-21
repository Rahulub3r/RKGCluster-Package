#' Class predictor of a K-Means Clustering using K-Nearest Neighbors
#'
#' Predicts target value in test data for a given training and test data
#'
#' @param train A training data matrix
#' @param test A test data matrix
#' @param k Number of clusters for K-Means to be used with a default of 2 clusters
#' @param train_target A vector which is the target column from the training dataset
#' @param test_target A vector which is required to build a confusion matrix and accuracies
#' @return \code{ClassPredictorK} returns an object of the class \code{'ClassPredictorK'} which has a vector of the class predicted, Confusion Matrix, Accuracy, False Positives and False Negatives. To see available attributes to pull type \code{names(lk)} where \code{lk} is an object of the class \code{ClassPredictorK}
#' @examples
#' #This example takes in age, height and weight and, targets as genders which are train[,5] and test[,5]
#' #and gives various predictions
#' train = read.csv('train.csv')
#' test = read.csv('test.csv')
#' lk = ClassPredictorK(train[,1:3], test[,1:3], k = 2, train[,5], test[,5])
#' names(lk)
#' @export

ClassPredictorK <- function(train, test, k = 2, train_target, test_target){
  require(clue)
  require(class)

  model = kmeans(train, k)
  temptrain = train
  temptrain$cluster = model$cluster

  temptest = test
  temptest$cluster = cl_predict(model, temptest)

  clusters = length(unique(model$cluster))

  l = list()

  for(i in 1:clusters){
    temptrain1 = subset(train, temptrain$cluster == i)
    temptest1 = subset(test, temptest$cluster == i)

    temp_train_target = subset(train_target, temptrain$cluster == i)
    temp_test_target = subset(test_target, temptest$cluster == i)

    temppredictions = knn(temptrain1, temptest1, cl = temp_train_target, k = ceiling(sqrt(nrow(temptrain1))))

    temppredictions = as.character(temppredictions)
    l$classes = c(l$classes, temppredictions)
  }

  l$confusion_matrix = table(l$classes, test_target)
  l$accuracy = 100*sum(diag(l$confusion_matrix))/sum(l$confusion_matrix)

  l$FalsePositiveRate = 100*l$confusion_matrix[1,2]/sum(l$confusion_matrix)
  l$FalseNegativeRate = 100*l$confusion_matrix[2,1]/sum(l$confusion_matrix)

  return(l)
}

#' Class predictor of a Guassian Mixture Model using K-Nearest Neighbors
#'
#' Predicts target value in test data for a given training and test data
#'
#' @param train A training data matrix
#' @param test A test data matrix
#' @param k Number of clusters for Guassian Mixture Model to be used with a default of 7 clusters
#' @param train_target A vector which is the target column from the training dataset
#' @param test_target A vector which is required to build a confusion matrix and accuracies
#' #' @return \code{ClassPredictorM} returns an object of the class \code{'ClassPredictorM'} which has a vector of the class predicted, Confusion Matrix, Accuracy, False Positives and False Negatives. To see available attributes to pull type \code{names(lm)} where \code{lm} is an object of the class \code{ClassPredictorM}
#' @examples
#' #This example takes in age, height and weight and, targets as genders which are train[,5] and test[,5]
#' #and gives various predictions
#' train = read.csv('train.csv')
#' test = read.csv('test.csv')
#' lm = ClassPredictorM(train[,1:3], test[,1:3], k = 7, train[,5], test[,5])
#' names(lm)
#' @export

ClassPredictorM <- function(train, test, k = 7, train_target, test_target){
  require(clue)
  require(class)
  require(mclust)

  model = Mclust(train, k)

  temptrain = train
  temptrain$cluster = model$classification

  temptest = test
  temptest$cluster = cl_predict(model, temptest)

  clusters = length(unique(model$classification))

  l = list()

  for(i in 1:clusters){
    temptrain1 = subset(train, temptrain$cluster == i)
    temptest1 = subset(test, temptest$cluster == i)

    temp_train_target = subset(train_target, temptrain$cluster == i)
    temp_test_target = subset(test_target, temptest$cluster == i)

    temppredictions = knn(temptrain1, temptest1, cl = temp_train_target, k = ceiling(sqrt(nrow(temptrain1))))

    temppredictions = as.character(temppredictions)

    l$classes = c(l$classes, temppredictions)
  }
  l$confusion_matrix = table(l$classes, test_target)
  l$accuracy = sum(diag(l$confusion_matrix))/sum(l$confusion_matrix)

  l$FalsePositiveRate = 100*l$confusion_matrix[1,2]/sum(l$confusion_matrix)
  l$FalseNegativeRate = 100*l$confusion_matrix[2,1]/sum(l$confusion_matrix)

  return(l)
}
