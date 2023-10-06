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

# One-Hot Encoding President Name variable

presTarget.train.BOW.OH = to_categorical(presTarget.train.BOW)
presTarget.train.TF.IDF.OH = to_categorical(presTarget.train.TF.IDF)

presTarget.val.BOW.OH = to_categorical(presTarget.val.BOW)
presTarget.val.TF.IDF.OH = to_categorical(presTarget.val.TF.IDF)

presTarget.test.BOW.OH = to_categorical(presTarget.test.BOW)
presTarget.test.TF.IDF.OH = to_categorical(presTarget.test.TF.IDF)

# Creating Feed-Forward Neural Network Model

dimensions = dim(presData.train.BOW)
presNum = dim(presTarget.test.BOW.OH)[2]

bowFit.FF.NN1 = keras_model_sequential()

bowFit.FF.NN1 %>% 
  layer_dense(units = dimensions[1]/10, 
              activation = "relu", 
              input_shape = dimensions[2]) %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = presNum, activation = "softmax")

bowFit.FF.NN1 %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_adam(learning_rate = 0.01),
  metrics = c('accuracy'),
)

bowFit.FF.NN1.History = bowFit.FF.NN1 %>% fit(
  presData.train.BOW, presTarget.train.BOW.OH, 
  epochs = 10, batch_size = 10,
  #validation_split = 0.1,
  validation_data = list(presData.val.BOW, presTarget.val.BOW.OH),
  verbose = 1, shuffle = TRUE
)

