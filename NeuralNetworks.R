rm(list = ls())
### Libraries

library(tidyverse)
library(tidytext)
library(tokenizers)
library(gghighlight)
library(tictoc)
library(keras)
library(reticulate)
library(tensorflow)

### Load Different Datasets

load("SetSentenceData.RData")
set.seed(2023)

## Neural Networks

# Pre-processing the Target Variable

presTarget.train.BOW = as.integer(factor(trainingSentencesBOW$presidentName)) - 1
presTarget.train.TF.IDF = as.integer(factor(trainingSentences.TF.IDF$presidentName)) - 1

presTarget.val.BOW = as.integer(factor(validationSentencesBOW$presidentName)) - 1
presTarget.val.TF.IDF = as.integer(factor(validationSentences.TF.IDF$presidentName)) - 1

presTarget.test.BOW = as.integer(factor(testSentencesBOW$presidentName)) - 1
presTarget.test.TF.IDF = as.integer(factor(testSentences.TF.IDF$presidentName)) - 1

# Removing President Variable from Data sets and scale Bag-Of-Words Data

presData.train.BOW = trainingSentencesBOW %>% 
  select(-presidentName) %>% 
  scale()

presData.train.TF.IDF = trainingSentences.TF.IDF %>% 
  select(-presidentName) %>%
  as.matrix()

presData.val.BOW = validationSentencesBOW %>% 
  select(-presidentName) %>%
  scale(center = attr(presData.train.BOW, "scaled:center"), 
        scale = attr(presData.train.BOW, "scaled:scale"))

presData.val.TF.IDF = validationSentences.TF.IDF %>% 
  select(-presidentName) %>%
  as.matrix()

presData.test.BOW = testSentencesBOW %>% 
  select(-presidentName) %>%
  scale(center = attr(presData.train.BOW, "scaled:center"), 
        scale = attr(presData.train.BOW, "scaled:scale"))

presData.test.TF.IDF = testSentences.TF.IDF %>% 
  select(-presidentName) %>%
  as.matrix()





