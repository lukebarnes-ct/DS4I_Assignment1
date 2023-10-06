rm(list = ls())
### Libraries

library(tidyverse)
library(tidytext)
library(tokenizers)
library(gghighlight)
library(tictoc)
library(nnet)

### Load Different Datasets

load("SetSentenceData.RData")

## Multinomial Logistic Regression

trainingSentencesBOW$presidentName = factor(trainingSentencesBOW$presidentName)
trainingSentences.TF.IDF$presidentName = factor(trainingSentences.TF.IDF$presidentName)

tic()
bowFit.MultiN = multinom(presidentName ~ ., 
                         data = trainingSentencesBOW)
toc()

tic()
tf.idf.Fit.MultiN = multinom(presidentName ~ ., 
                             data = trainingSentences.TF.IDF[, -c(1:10000)])
toc()