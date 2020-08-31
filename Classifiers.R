library(dplyr)

## Naive Bayes Classifier Using for Iris Data

iris_nb <- function(testx, trainx, trainy){
  
  train_data <- data.frame(trainx,trainy)
  
  #Find the mean
  xbar_sepLength <- train_data %>% group_by(trainy) %>% summarize(mean(Sepal.Length))
  xbar_sepWidth <- train_data %>% group_by(trainy) %>% summarize(mean(Sepal.Width))
  xbar_petLength <- train_data %>% group_by(trainy) %>% summarize(mean(Petal.Length))
  xbar_petWidth <- train_data %>% group_by(trainy) %>% summarize(mean(Petal.Width))
  
  #Find sd
  sd_sepLength <- train_data %>% group_by(trainy) %>% summarize(sd(Sepal.Length))
  sd_sepWidth <- train_data %>% group_by(trainy) %>% summarize(sd(Sepal.Width))
  sd_petLength <- train_data %>% group_by(trainy) %>% summarize(sd(Petal.Length))
  sd_petWidth <- train_data %>% group_by(trainy) %>% summarize(sd(Petal.Width))
  
  
  #Likelihood
  
  like_setosa <- dnorm(testx[1], mean = xbar_sepLength[[1,2]], sd = sd_sepLength[[1,2]]) * 
    dnorm(testx[2], mean = xbar_sepWidth[[1,2]], sd = sd_sepWidth[[1,2]]) * 
    dnorm(testx[3], xbar_petLength[[1,2]], sd_petLength[[1,2]]) * 
    dnorm(testx[4], xbar_petWidth[[1,2]], sd_petWidth[[1,2]])
  
  like_versi <- dnorm(testx[1], xbar_sepLength[[2,2]], sd_sepLength[[2,2]]) * 
    dnorm(testx[2], xbar_sepWidth[[2,2]], sd_sepWidth[[2,2]]) *
    dnorm(testx[3], xbar_petLength[[2,2]], sd_petLength[[2,2]]) * 
    dnorm(testx[4], xbar_petWidth[[2,2]], sd_petWidth[[2,2]])
  
  like_virg <- dnorm(testx[1], xbar_sepLength[[3,2]], sd_sepLength[[3,2]]) * 
    dnorm(testx[2], xbar_sepWidth[[3,2]], sd_sepWidth[[3,2]]) * 
    dnorm(testx[3], xbar_petLength[[3,2]], sd_petLength[[3,2]]) * 
    dnorm(testx[4], xbar_petWidth[[3,2]], sd_petWidth[[3,2]])
  
  #Prior
  
  #Find the total and count of each class
  total <- nrow(train_data)
  each_amount <- train_data %>% group_by(trainy) %>% summarize(count = n())
  
  prior_setosa <- each_amount$count[1] / total
  prior_versi <- each_amount$count[2] / total
  prior_virg <- each_amount$count[3] / total
  
  #Marginal
  
  marginal <- like_setosa*prior_setosa + like_versi*prior_versi + like_virg*prior_virg
  
  #Posterior Class Probabilities
  setosa <- like_setosa * prior_setosa / marginal
  versicolor <- like_versi * prior_versi / marginal
  virginica <- like_virg * prior_versi / marginal
  
  result <- data.frame(setosa, versicolor, virginica)
  
  result
}


## Testing it out

set.seed(1)
training_rows <- sort(c(sample(1:50, 40), sample(51:100, 40), sample(101:150, 40)))
training_x <- as.matrix(iris[training_rows, 1:4])
training_y <- iris[training_rows, 5]

# test cses
test_case_a <- as.matrix(iris[24, 1:4]) # true class setosa
test_case_b <- as.matrix(iris[73, 1:4]) # true class versicolor
test_case_c <- as.matrix(iris[124, 1:4]) # true class virginica

# class predictions of test cases
iris_nb(test_case_a, training_x, training_y)
iris_nb(test_case_b, training_x, training_y)
iris_nb(test_case_c, training_x, training_y)

## Using the Naive Bayes Function that already exists in R
library(e1071)
nb_model1 <- naiveBayes(training_x, training_y)
predict(nb_model1, newdata = test_case_a, type = 'raw')
predict(nb_model1, newdata = test_case_b, type = 'raw')
predict(nb_model1, newdata = test_case_c, type = 'raw')

nb_model2 <- naiveBayes(training_x2, training_y2)
predict(nb_model2, newdata = test_case_a, type = 'raw')
predict(nb_model2, newdata = test_case_b, type = 'raw')
predict(nb_model2, newdata = test_case_c, type = 'raw')



## K-nearest neighbors Classifier for the Iris data

distance <- function(a, b){
  total <- sum((a-b)^2)
  sqrt(total)
}

iris_knn <- function(testx, trainx, trainy, k){
  
  #find euclidean distance
  euclidean <- rep(NA, nrow(trainx))
  
  for(i in seq(1:nrow(trainx))){
    euclidean[i] <- distance(testx, trainx[i,]) 
  }
  
  result <- data.frame(trainx,trainy,euclidean)
  
  #sort the dataframe by distance
  result <- result %>% arrange(euclidean)
  
  #Take the K nearest points
  k_nearest <- result$trainy[1:k]
  order <- names(sort(table(k_nearest), decreasing = TRUE)) #gets the frequency then sorts. 
  
  order[1] #return the max
  
}

## Test
iris_knn(test_case_a, training_x, training_y, 5)
iris_knn(test_case_b, training_x, training_y, 5) 
iris_knn(test_case_c, training_x, training_y, 5)

iris_knn(test_case_a, training_x2, training_y2, 5)
iris_knn(test_case_b, training_x2, training_y2, 5)
iris_knn(test_case_c, training_x2, training_y2, 5) 

## Using KNN function that already exists in R

library(class)
knn(train = training_x, cl = training_y, test = test_case_a, k = 5)
knn(train = training_x, cl = training_y, test = test_case_b, k = 5) 
knn(train = training_x, cl = training_y, test = test_case_c, k = 5)

knn(train = training_x2, cl = training_y2, test = test_case_a, k = 5)
knn(train = training_x2, cl = training_y2, test = test_case_b, k = 5)
knn(train = training_x2, cl = training_y2, test = test_case_c, k = 5) 

