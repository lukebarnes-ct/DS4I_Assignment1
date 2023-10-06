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

### Load Data for Plots and Tables

load("T&RF.RData")

## Classification Tree

tic()
bowFit = rpart(presidentName ~., 
               data = trainingSentencesBOW,
               method = "class")
toc()

trainFittedBOW = predict(bowFit, type = 'class')
trainPredBOW = table(trainingSentencesBOW$presidentName, trainFittedBOW)
classErrorBOW.CT = (1 - round(sum(diag(trainPredBOW))/sum(trainPredBOW), 3))

tic()
tf.idf.Fit = rpart(presidentName ~., 
                   data = trainingSentences.TF.IDF,
                   method = "class")
toc()

trainFitted.TF.IDF = predict(tf.idf.Fit, type = 'class')
trainPred.TF.IDF = table(trainingSentences.TF.IDF$presidentName, 
                         trainFitted.TF.IDF)
classError.TF.IDF.CT = (1 - round(sum(diag(trainPred.TF.IDF))/sum(trainPred.TF.IDF), 3))

## Random Forests (using Ranger)

trainingSentencesBOW$presidentName = factor(trainingSentencesBOW$presidentName)
trainingSentences.TF.IDF$presidentName = factor(trainingSentences.TF.IDF$presidentName)

#### Validation Analysis with Ranger
#### tf-IDF

error = c()
valErr = c()
seq = seq(100, 3000, by = 100)
for(i in 1:length(seq)){
  tic()
  mod = ranger(presidentName ~ ., 
               data = trainingSentences.TF.IDF,
               num.trees = seq[i], 
               mtry = 20)
  error[i] = mod$prediction.error
  valModPreds = predict(mod, validationSentences.TF.IDF)$predictions
  valModPreds.Ranger = table(validationSentences.TF.IDF$presidentName, valModPreds)
  valErr[i] = (1 - round(sum(diag(valModPreds.Ranger))/sum(valModPreds.Ranger), 3))
  print(paste0("Iteration: ", i))
  toc()
}

valPlotData.TF.IDF = data.frame("Seq" = seq,
                                "Error" = error,
                                "ValError" = valErr)

minTrees.TF.IDF.RF = seq[which.min(valErr)]

ggplot(valPlotData.TF.IDF, aes(x = Seq)) +
  geom_line(aes(y = Error), linewidth = 3, linetype = 1, col = "black") +
  geom_line(aes(y = ValError), linewidth = 3, linetype = 1, col = "red") +
  geom_vline(xintercept = minTrees.TF.IDF.RF, 
             linewidth = 1, linetype = 6, col = "blue") +
  xlab("Number of Trees") + ylab("Classification Error") +
  theme_bw(base_size = 14)

#### Validation Analysis with Ranger
#### Bag-Of-Words

errorBOW = c()
valErrBOW = c()
for(i in 1:length(seq)){
  tic()
  mod = ranger(presidentName ~ ., 
               data = trainingSentencesBOW,
               num.trees = seq[i], 
               mtry = 20)
  errorBOW[i] = mod$prediction.error
  valModPredsBOW = predict(mod, validationSentencesBOW)$predictions
  valModPredsBOW.Ranger = table(validationSentencesBOW$presidentName, 
                                valModPredsBOW)
  valErrBOW[i] = (1 - round(sum(diag(valModPredsBOW.Ranger))/sum(valModPredsBOW.Ranger), 3))
  print(paste0("Iteration: ", i))
  toc()
}

valPlotDataBOW = data.frame("Seq" = seq,
                            "Error" = errorBOW,
                            "ValError" = valErrBOW)

minTreesBOW = seq[which.min(valErrBOW)]

ggplot(valPlotDataBOW, aes(x = Seq)) +
  geom_line(aes(y = Error), linewidth = 3, linetype = 1, col = "black") +
  geom_line(aes(y = ValError), linewidth = 3, linetype = 1, col = "red") +
  geom_vline(xintercept = minTreesBOW, 
             linewidth = 1, linetype = 6, col = "blue") +
  xlab("Number of Trees") + ylab("Classification Error") +
  theme_bw(base_size = 14)

#### Fit RF with Best Parameters on Training and Val Sets together

tAndVSentencesBOW = as.data.frame(rbind(trainingSentencesBOW, 
                                        validationSentencesBOW))
tic()
bowFit.Ranger = ranger(presidentName ~ ., 
                       data = tAndVSentencesBOW,
                       mtry = 20,
                       num.trees = minTreesBOW)
toc()

trainFittedBOW.Ranger = bowFit.Ranger$predictions
trainPredBOW.Ranger = table(tAndVSentencesBOW$presidentName, 
                            trainFittedBOW.Ranger)
classErrorBOW.RF = round(bowFit.Ranger$prediction.error, 3)

tAndVSentences.TF.IDF = as.data.frame(rbind(trainingSentences.TF.IDF, 
                                            validationSentences.TF.IDF))
tic()
tf.idf.Fit.Ranger = ranger(presidentName ~ ., 
                           data = tAndVSentences.TF.IDF,
                           mtry = 20,
                           num.trees = minTrees.TF.IDF.RF)
toc()

trainFitted.TF.IDF.Ranger = tf.idf.Fit.Ranger$predictions
trainPred.TF.IDF.Ranger = table(tAndVSentences.TF.IDF$presidentName, trainFitted.TF.IDF.Ranger)
classError.TF.IDF.RF = round(tf.idf.Fit.Ranger$prediction.error, 3)

##### Use on Test Data [ONLY ONCE]

bow.Test.CT = predict(bowFit, testSentencesBOW, type = 'class')
bow.Test.CT.Tab = table(testSentencesBOW$presidentName, bow.Test.CT)
bow.Test.CT.ClassError = (1 - round(sum(diag(bow.Test.CT.Tab))/sum(bow.Test.CT.Tab), 3))

bow.Test.RF = predict(bowFit.Ranger, testSentencesBOW)$predictions
bow.Test.RF.Tab = table(testSentencesBOW$presidentName, bow.Test.RF)
bow.Test.RF.ClassError = (1 - round(sum(diag(bow.Test.RF.Tab))/sum(bow.Test.RF.Tab), 3))

tf.idf.Test.CT = predict(tf.idf.Fit, testSentences.TF.IDF, type = 'class')
tf.idf.Test.CT.Tab = table(testSentences.TF.IDF$presidentName, tf.idf.Test.CT)
tf.idf.Test.CT.ClassError = (1 - round(sum(diag(tf.idf.Test.CT.Tab))/sum(tf.idf.Test.CT.Tab), 3))

tf.idf.Test.RF = predict(tf.idf.Fit.Ranger, testSentences.TF.IDF)$predictions
tf.idf.Test.RF.Tab = table(testSentences.TF.IDF$presidentName, tf.idf.Test.RF)
tf.idf.Test.RF.ClassError = (1 - round(sum(diag(tf.idf.Test.RF.Tab))/sum(tf.idf.Test.RF.Tab), 3))

## Save Output for use later

save(trainFittedBOW, trainPredBOW, classErrorBOW.CT,
     trainFitted.TF.IDF, trainPred.TF.IDF, classError.TF.IDF.CT,
     valPlotData.TF.IDF, minTrees.TF.IDF.RF,
     valPlotDataBOW, minTreesBOW,
     trainFittedBOW.Ranger, 
     trainPredBOW.Ranger, classErrorBOW.RF,
     trainFitted.TF.IDF.Ranger,
     trainPred.TF.IDF.Ranger, classError.TF.IDF.RF,
     bow.Test.CT, bow.Test.CT.Tab, bow.Test.CT.ClassError,
     bow.Test.RF, bow.Test.RF.Tab, bow.Test.RF.ClassError,
     tf.idf.Test.CT, tf.idf.Test.CT.Tab, tf.idf.Test.CT.ClassError,
     tf.idf.Test.RF, tf.idf.Test.CT.Tab, tf.idf.Test.RF.ClassError,
     file = "T&RF.RData")