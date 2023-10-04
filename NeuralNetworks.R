rm(list = ls())
### Libraries

library(tidyverse)
library(tidytext)
library(tokenizers)
library(gghighlight)
library(rpart)
library(tictoc)
library(ranger)
library(caret)

### Load Different Datasets

load("SetSentenceData.RData")

## Classification Tree

tic()
bowFit = rpart(presidentName ~., 
               data = trainingSentencesBOW,
               method = "class")
toc()

trainFittedBOW = predict(bowFit, type = 'class')
trainPredBOW = table(trainingSentencesBOW$presidentName, trainFittedBOW)
round(sum(diag(trainPredBOW))/sum(trainPredBOW), 3)

tic()
tf.idf.Fit = rpart(presidentName ~., 
                   data = trainingSentences.TF.IDF,
                   method = "class")
toc()

trainFitted.TF.IDF = predict(tf.idf.Fit, type = 'class')
trainPred.TF.IDF = table(trainingSentences.TF.IDF$presidentName, 
                         trainFitted.TF.IDF)
round(sum(diag(trainPred.TF.IDF))/sum(trainPred.TF.IDF), 3)

## Random Forests

trainingSentencesBOW$presidentName = factor(trainingSentencesBOW$presidentName)

tic()
bowFit.Ranger = ranger(presidentName ~ ., 
                       data = trainingSentencesBOW,
                       mtry = 20,
                       num.trees = 500)
toc()

trainFittedBOW.Ranger = bowFit.Ranger$predictions
trainPredBOW.Ranger = table(trainingSentencesBOW$presidentName, trainFittedBOW.Ranger)
round(sum(diag(trainPredBOW.Ranger))/sum(trainPredBOW.Ranger), 3)

trainingSentences.TF.IDF$presidentName = factor(trainingSentences.TF.IDF$presidentName)

tic()
tf.idf.Fit.Ranger = ranger(presidentName ~ ., 
                           data = trainingSentences.TF.IDF,
                           mtry = 20,
                           num.trees = 500)
toc()

trainFitted.TF.IDF.Ranger = tf.idf.Fit.Ranger$predictions
trainPred.TF.IDF.Ranger = table(trainingSentences.TF.IDF$presidentName, trainFitted.TF.IDF.Ranger)
round(sum(diag(trainPred.TF.IDF.Ranger))/sum(trainPred.TF.IDF.Ranger), 3)

#### Validation Analysis with Ranger

error = c()
seq = seq(100, 500, by = 100)
for(i in 1:length(seq)){
  tic()
  error[i] = ranger(presidentName ~ ., 
                    data = trainingSentences.TF.IDF,
                    num.trees = seq[i], 
                    mtry = 20)$prediction.error
  print(paste0("Iteration: ", i))
  toc()
}

plot(seq, error, type = 'l', col = 'blue', lwd = 3,)

rf_grid = expand.grid(mtry = seq(5, 25, by = 5),
                      splitrule = 'gini',
                      min.node.size = 1)

ctrl = trainControl(method = 'oob', verboseIter = T)

tic()
rf_gridsearch = train(presidentName ~ ., 
                      data = trainingSentences.TF.IDF,
                      method = 'ranger',
                      num.trees = 100,
                      verbose = T,
                      trControl = ctrl,
                      tuneGrid = rf_grid)
toc()

res = rf_gridsearch$results
rf_gridsearch$finalModel


## Neural Networks


  
